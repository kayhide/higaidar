module Component.UserShowUI where

import Prelude

import Api as Api
import Api.Users as Users
import Component.HTML.Checkbox as Checkbox
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.HTML.TextField as TextField
import Component.Util as Util
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (runExceptT)
import Data.DateTime.Locale (Locale)
import Data.Either (Either(Left, Right))
import Data.Lens (Lens', _Just, assign, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n as I18n
import I18n.Ja as Ja
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
    [ HH.text Ja.user ]
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
    renderForm =
      HH.div
      [ HP.class_ $ H.ClassName "col-md-12 mb-4" ]
      [
        renderInput "user-code" "Code" User._code
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
      [ HH.text Ja.submit ]

    renderItem (User { id, code, tel, name, is_admin, created_at, updated_at }) =
      HH.div
      [ HP.class_ $ H.ClassName "col-sm-6 mb-2" ]
      [
        HH.div
        [ HP.class_ $ H.ClassName "card" ]
        [
          HH.div
          [ HP.class_ $ H.ClassName "card-body" ]
          [
            HH.div
            [ HP.class_ $ H.ClassName "card-text small" ]
            [ HH.text name ]
          , HH.div
            [ HP.class_ $ H.ClassName "card-text small" ]
            [ HH.text code ]
          , HH.div
            [ HP.class_ $ H.ClassName "card-text small" ]
            [ HH.text tel ]
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
    Util.whenNotBusy_ do
      { client, userId } <- H.get
      res <- runExceptT do
        Util.onLeft "Failed to load user."
          =<< (H.liftAff $ attempt $ Users.find client userId)

      case res of
        Right user ->
          H.modify _{ user = Just user, editing = Just user }
        Left msg ->
          H.raise $ Failed msg

    pure next

  SetString attr v next -> do
    assign (_editing <<< _Just <<< attr) v
    pure next

  SetBoolean attr v next -> do
    assign (_editing <<< _Just <<< attr) v
    pure next

  Submit next -> do
    Util.whenNotBusy_ do
      { client } <- H.get
      res <- runExceptT do
        editing <- Util.onNothing "User not loaded." =<< (H.gets _.editing)
        Util.onLeft "Failed to update user."
          =<< (H.liftAff $ attempt $ Users.update client editing)

      case res of
        Right user ->
          H.modify _{ user = Just user, editing = Just user }
        Left msg ->
          H.raise $ Failed msg

    pure next
