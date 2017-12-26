module Component.LoginUI where

import Prelude

import Api as Api
import Api.Token as Token
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.HTML.TextField as TextField
import Component.Util as Util
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(Left, Right))
import Data.Lens (Lens', assign, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Symbol (SProxy(..))
import Dom.Storage (STORAGE)
import Dom.Storage as Storage
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n.Ja as Ja
import Network.HTTP.Affjax (AJAX)


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


type State =
  { client :: Api.Client
  , form :: Api.AuthenticateForm
  , name :: Maybe String
  , busy :: Boolean
  }

_form :: Lens' State Api.AuthenticateForm
_form = prop (SProxy :: SProxy "form")

data Query a
  = Initialize a
  | Authenticate a
  | Unauthenticate a
  | SetString (Lens' Api.AuthenticateForm String) String a
  | SetCode String a
  | SetTel String a

type Input = Api.Client

data Message
  = Authenticated Api.Client
  | Unauthenticated Api.Client
  | Failed Api.Client Api.AuthenticateForm String


type Eff_ eff = Aff (ajax :: AJAX, storage :: STORAGE | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where
    initialState client =
      { client
      , form: Api.AuthenticateForm { code: "", tel: "" }
      , name: Nothing
      , busy: false
      }

render :: State -> H.ComponentHTML Query
render state@({ form: Api.AuthenticateForm { code, tel } }) =
  HH.div_
  [
    HH.h1_
    [ HH.text Ja.login ]
  , LoadingIndicator.render state.busy
  , renderForm
  ]

  where
    isAuthenticated = Api.isAuthenticated state.client

    renderForm =
      HH.div
      [ HP.class_ $ H.ClassName "form" ]
      [
        renderInput "user-code" Ja.user_code Api._code
      , renderInput "user-tel" Ja.user_tel Api._tel
      , HH.hr_
      , HH.div
        [ HP.class_ $ H.ClassName "d-flex mt-2" ]
        [
          renderLoginButton
        , renderLogoutButton
        ]
      ]

    renderInput :: String -> String -> Lens' Api.AuthenticateForm String -> H.ComponentHTML Query
    renderInput key label attr =
      TextField.render key label (view attr state.form) $ SetString attr

    renderLoginButton =
      HH.button
      [ HP.class_ $ H.ClassName $ "btn btn-outline-secondary" <> if isAuthenticated then " disabled" else ""
      , HE.onClick $ HE.input_ Authenticate
      ]
      [
        HH.i [ HP.class_ $ H.ClassName "fa fa-sign-in fa-fw mr-1" ] []
      , HH.text Ja.login
      ]

    renderLogoutButton =
      HH.button
      [ HP.class_ $ H.ClassName $ "ml-auto btn btn-secondary" <> if isAuthenticated then "" else " disabled"
      , HE.onClick $ HE.input_ Unauthenticate
      ]
      [
        HH.i [ HP.class_ $ H.ClassName "fa fa-sign-out fa-fw mr-1" ] []
      , HH.text Ja.logout
      ]

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    form <- H.liftAff $ attempt loadForm
    case form of
      Right form_ -> do
        H.modify _{ form = form_ }
        isAuthenticated <- Api.isAuthenticated <$> H.gets _.client
        case isAuthenticated of
          true -> pure next
          false -> eval $ Authenticate next
      Left s ->
        pure next

  Authenticate next -> do
    Util.whenNotBusy_ do
      Api.Client { endpoint } <- H.gets _.client
      form <- H.gets _.form
      let cli = Api.makeClient endpoint

      res <- runExceptT do
        Util.onLeft "Authentication failed"
          =<< (H.liftAff $ attempt $ Token.authenticate cli form)

      case res of
        Right cli_ -> do
          H.modify _{ client = cli_ }
          H.liftAff $ saveForm form
          H.raise $ Authenticated cli_

        Left s -> do
          H.modify _{ client = cli }
          H.raise $ Failed cli form s

    pure next

  Unauthenticate next -> do
    Api.Client { endpoint } <- H.gets _.client
    let cli = Api.makeClient endpoint
    H.modify _{ client = cli }
    H.raise $ Unauthenticated $ cli

    pure next

  SetString attr v next -> do
    assign (_form <<< attr) v
    pure next

  SetCode code next -> do
    Api.AuthenticateForm form <- H.gets _.form
    H.modify $ _{ form = Api.AuthenticateForm $ form { code = code } }
    pure next

  SetTel tel next -> do
    Api.AuthenticateForm form <- H.gets _.form
    H.modify $ _{ form = Api.AuthenticateForm $ form { tel = tel } }
    pure next


loadForm :: forall eff. Aff (storage :: STORAGE | eff) Api.AuthenticateForm
loadForm = do
  code <- Storage.get "user.code"
  tel <- Storage.get "user.tel"
  pure $ Api.AuthenticateForm { code, tel }

saveForm :: forall eff. Api.AuthenticateForm -> Aff (storage :: STORAGE | eff) Unit
saveForm (Api.AuthenticateForm { code, tel }) = do
  Storage.set "user.code" code
  Storage.set "user.tel" tel
