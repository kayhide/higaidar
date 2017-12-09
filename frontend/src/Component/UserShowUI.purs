module Component.UserShowUI where

import Prelude

import Api.Token (AuthenticationToken)
import Api.Users as Users
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.HTML.TextField as TextField
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Data.DateTime as DateTime
import Data.DateTime.Locale (Locale(Locale))
import Data.Either (Either(..), either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Int (fromString)
import Data.Lens (Lens', _Just, assign, lens, set, view, (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model.User (User(User))
import Model.User as User
import Network.HTTP.Affjax (AJAX, URL)
import Route as R


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


data Query a
  = Initialize a
  | SetLocale Locale a
  | Reload a
  | SetAttr (Lens' User String) String a
  | Submit a

type State =
  { userId :: Int
  , user :: Maybe User
  , editing :: Maybe User
  , baseUrl :: URL
  , token :: AuthenticationToken
  , locale :: Locale
  , busy :: Boolean
  }

_editing :: Lens' State (Maybe User)
_editing = prop (SProxy :: SProxy "editing")


type Input =
  { userId :: Int
  , baseUrl :: URL
  , token :: AuthenticationToken
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
initialState { userId, baseUrl, token, locale } =
    { userId, baseUrl, token, locale
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

    renderForm =
      HH.form
      [ HP.class_ $ H.ClassName "col-md-12 mb-4" ]
      [
        renderInput "user-code" "Code" _User_code
      , renderInput "user-name" "Name" User._name
      , renderInput "user-tel" "Tel" User._tel
      , renderSubmitButton
      ]

    renderInput :: String -> String -> Lens' User String -> H.ComponentHTML Query
    renderInput key label attr =
      TextField.render key label (state.editing ^. _Just <<< attr) $ SetAttr attr

    renderSubmitButton =
      HH.button
      [ HP.class_ $ H.ClassName "btn btn-primary"
      , HE.onClick $ HE.input_ Submit
      ]
      [ HH.text "Submit" ]

    renderItem (User { id, code, tel, name, created_at, updated_at }) =
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
              [ HP.class_ $ H.ClassName "fa fa-user mr-2"
              , HP.title name
              ] []
            , HH.a
              [ HP.href $ R.path $ R.UsersShow id ]
              [
                HH.text name
              ]
            ]
          , HH.div
            [ HP.class_ $ H.ClassName "card-text small" ]
            [ HH.text $ show code ]
          , HH.div
            [ HP.class_ $ H.ClassName "card-text small" ]
            [ HH.text $ tel ]
          , HH.div
            [ HP.class_ $ H.ClassName "card-text text-muted small" ]
            [ renderDateTime created_at state.locale ]
          , HH.div
            [ HP.class_ $ H.ClassName "card-text text-muted small" ]
            [ renderDateTime updated_at state.locale ]
          ]
        ]
      ]

    renderDateTime dt (Locale _ dur) =
      HH.text $ either id id $ maybe (Left "") (formatDateTime "YYYY/MM/DD HH:mm:ss") dt_
      where
        dt_ = (DateTime.adjust (negate dur)) dt

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
      url <- H.gets _.baseUrl
      token <- H.gets _.token
      userId <- H.gets _.userId
      res <- runExceptT do
        user <- onLeft "Failed to access api"
                =<< (H.liftAff $ attempt $ Users.find url token userId)
        lift do
          H.modify _{ user = Just user, editing = Just user }

      either (H.raise <<< Failed) pure res

    H.modify _{ busy = false }
    pure next

  SetAttr attr v next -> do
    assign (_editing <<< _Just <<< attr) v
    pure next

  Submit next -> do
    pure next

onLeft :: forall e m. Monad m => String -> Either e ~> ExceptT String m
onLeft s = either (throwError <<< const s) pure
