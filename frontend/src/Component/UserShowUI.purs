module Component.UserShowUI where

import Prelude

import Api as Api
import Api.Users as Users
import Component.Admin.Route as R
import Component.HTML.Checkbox as Checkbox
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.HTML.TextField as TextField
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Data.DateTime.Locale (Locale)
import Data.Either (Either, either)
import Data.Int (fromString)
import Data.Lens (Lens', _Just, assign, lens, set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n as I18n
import Model.User (User(User))
import Model.User as User
import Network.HTTP.Affjax (AJAX)


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


data Query a
  = Initialize a
  | SetLocale Locale a
  | Reload a
  | SetString (Lens' User String) String a
  | SetBoolean (Lens' User Boolean) Boolean a
  | Submit a

type State =
  { userId :: Int
  , user :: Maybe User
  , editing :: Maybe User
  , client :: Api.Client
  , locale :: Locale
  , busy :: Boolean
  }

_editing :: Lens' State (Maybe User)
_editing = prop (SProxy :: SProxy "editing")


type Input =
  { userId :: Int
  , client :: Api.Client
  , locale :: Locale
  }

data Message
  = Failed String


type Eff_ eff = Aff (ajax :: AJAX | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleComponent
    { initialState: initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

initialState :: Input -> State
initialState { userId, client, locale } =
    { userId, client, locale
    , user: Nothing
    , editing: Nothing
    , busy: false
    }

render :: State -> H.ComponentHTML Query
render state =
  HH.div_
  [
    HH.h1_
    [ HH.text "User" ]
  , LoadingIndicator.render state.busy
  , HH.div
    [ HP.class_ $ H.ClassName "row" ]
    [ renderForm ]
  , HH.div
    [ HP.class_ $ H.ClassName "row" ]
    $ maybe [] (pure <<< renderItem) state.editing
    <> maybe [] (pure <<< renderItem) state.user
  ]

  where
    _User_code :: Lens' User String
    _User_code = lens
                 (show <<< view User._code)
                 (\s -> maybe s (flip (set User._code) s) <<< fromString)

    _User_is_admin :: Lens' User String
    _User_is_admin = lens
                 (show <<< view User._is_admin)
                 (\s -> maybe s (flip (set User._code) s) <<< fromString)

    renderForm =
      HH.div
      [ HP.class_ $ H.ClassName "col-md-12 mb-4" ]
      [
        renderInput "user-code" "Code" _User_code
      , renderInput "user-name" "Name" User._name
      , renderInput "user-tel" "Tel" User._tel
      , renderCheckbox "user-is_admin" "Admin" User._is_admin
      , renderSubmitButton
      ]

    renderInput :: String -> String -> Lens' User String -> H.ComponentHTML Query
    renderInput key label attr =
      TextField.render key label (maybe "" (view attr) state.editing) $ SetString attr

    renderCheckbox :: String -> String -> Lens' User Boolean -> H.ComponentHTML Query
    renderCheckbox key label attr =
      Checkbox.render key label (maybe false (view attr) state.editing) $ SetBoolean attr

    renderSubmitButton =
      HH.button
      [ HP.class_ $ H.ClassName "btn btn-primary"
      , HE.onClick $ HE.input_ Submit
      ]
      [ HH.text "Submit" ]

    renderItem (User { id, code, tel, name, is_admin, created_at, updated_at }) =
      HH.div
      [ HP.class_ $ H.ClassName "col-sm-6 mb-2" ]
      [
        HH.div
        [ HP.class_ $ H.ClassName "card" ]
        [
          HH.div
          [ HP.class_ $ H.ClassName "card-body" ]
          [ HH.p
            [ HP.class_ $ H.ClassName "card-text small" ]
            [
              HH.i
              [ HP.class_ $ H.ClassName "fa fa-user mr-2" ] []
            , HH.a
              [ HP.href $ R.path $ R.UsersShow id ]
              [ HH.text name ]
            ]
          , HH.div
            [ HP.class_ $ H.ClassName "card-text small" ]
            [ HH.text $ show code ]
          , HH.div
            [ HP.class_ $ H.ClassName "card-text small" ]
            [ HH.text $ tel ]
          , HH.div
            [ HP.class_ $ H.ClassName "card-text small" ]
            [ HH.text $ show is_admin ]
          , HH.div
            [ HP.class_ $ H.ClassName "card-text text-muted small" ]
            [ HH.text $ I18n.localizeDateTime state.locale created_at ]
          , HH.div
            [ HP.class_ $ H.ClassName "card-text text-muted small" ]
            [ HH.text $ I18n.localizeDateTime state.locale updated_at ]
          ]
        ]
      ]

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    eval $ Reload next

  SetLocale locale next -> do
    H.modify _{ locale = locale }
    pure next

  Reload next -> do
    busy <- H.gets _.busy
    when (not busy) do
      H.modify _{ busy = true }
      cli <- H.gets _.client
      userId <- H.gets _.userId
      res <- runExceptT do
        user <- onLeft "Failed to access api"
                =<< (H.liftAff $ attempt $ Users.find cli userId)
        lift do
          H.modify _{ user = Just user, editing = Just user }

      either (H.raise <<< Failed) pure res

    H.modify _{ busy = false }
    pure next

  SetString attr v next -> do
    assign (_editing <<< _Just <<< attr) v
    pure next

  SetBoolean attr v next -> do
    assign (_editing <<< _Just <<< attr) v
    pure next

  Submit next -> do
    busy <- H.gets _.busy
    when (not busy) do
      editing <- H.gets _.editing
      case editing of
        Just editing_ -> do
          H.modify _{ busy = true }
          cli <- H.gets _.client
          userId <- H.gets _.userId
          res <- runExceptT do
            user <- onLeft "Failed to access api"
                    =<< (H.liftAff $ attempt $ Users.update cli editing_)
            lift do
              H.modify _{ user = Just user, editing = Just user }

          either (H.raise <<< Failed) pure res

        Nothing -> pure unit

      H.modify _{ busy = false }
    pure next

onLeft :: forall e m. Monad m => String -> Either e ~> ExceptT String m
onLeft s = either (throwError <<< const s) pure
