module Component.LoginUI where

import AppPrelude

import Api as Api
import Api.Token as Token
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.HTML.TextField as TextField
import Component.Util as Util
import Control.Monad.Except (runExceptT)
import Data.Lens (Lens', assign, view)
import Data.Lens.Record (prop)
import Dom.Storage as Storage
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n.Ja as Ja


type State =
  { client :: Api.Client
  , form :: Api.AuthenticateForm
  , name :: Maybe String
  , busy :: Boolean
  }

_form :: Lens' State Api.AuthenticateForm
_form = prop (SProxy :: SProxy "form")

data Action
  = Initialize
  | Authenticate
  | Unauthenticate
  | SetString (Lens' Api.AuthenticateForm String) String
  | SetCode String
  | SetTel String

type Input = Api.Client

data Message
  = Authenticated Api.Client
  | Unauthenticated Api.Client
  | Failed Api.Client Api.AuthenticateForm String


ui ::
  forall m.
  MonadAff m =>
  H.Component HH.HTML (Const Void) Input Message m
ui =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    }
  }
  where
    initialState client =
      { client
      , form: Api.AuthenticateForm { code: "", tel: "" }
      , name: Nothing
      , busy: false
      }

render ::
  forall m.
  MonadAff m =>
  State -> H.ComponentHTML Action () m
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
      , renderInputWithHelp "user-tel" Ja.user_tel Ja.without_hyphen Api._tel
      , HH.hr_
      , HH.div
        [ HP.class_ $ H.ClassName "d-flex mt-2" ]
        [ renderLoginButton
        , renderLogoutButton
        ]
      ]

    renderInput :: String -> String -> Lens' Api.AuthenticateForm String -> _
    renderInput key label attr =
      TextField.render key label (view attr state.form) $ SetString attr

    renderInputWithHelp :: String -> String -> String -> Lens' Api.AuthenticateForm String -> _
    renderInputWithHelp key label help attr =
      TextField.renderWithHelp key label (view attr state.form) help $ SetString attr

    renderLoginButton =
      HH.button
      [ HP.class_ $ H.ClassName $ "btn btn-outline-secondary" <> if isAuthenticated then " disabled" else ""
      , HE.onClick $ const $ Just Authenticate
      ]
      [ HH.i [ HP.class_ $ H.ClassName "fa fa-sign-in fa-fw mr-1" ] []
      , HH.text Ja.login
      ]

    renderLogoutButton =
      HH.button
      [ HP.class_ $ H.ClassName $ "ml-auto btn btn-secondary" <> if isAuthenticated then "" else " disabled"
      , HE.onClick $ const $ Just Unauthenticate
      ]
      [ HH.i [ HP.class_ $ H.ClassName "fa fa-sign-out fa-fw mr-1" ] []
      , HH.text Ja.logout
      ]

handleAction ::
  forall m.
  MonadAff m =>
  Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Initialize -> do
    form <- H.liftAff $ attempt loadForm
    case form of
      Right form_ -> do
        H.modify_ _{ form = form_ }
        isAuthenticated <- Api.isAuthenticated <$> H.gets _.client
        case isAuthenticated of
          true -> pure unit
          false -> handleAction $ Authenticate
      Left s ->
        pure unit

  Authenticate -> do
    Util.whenNotBusy_ do
      Api.Client { endpoint } <- H.gets _.client
      form <- H.gets _.form
      let cli = Api.makeClient endpoint

      res <- runExceptT do
        Util.onLeft "Authentication failed"
          =<< (H.liftAff $ attempt $ Token.authenticate cli form)

      case res of
        Right cli_ -> do
          H.modify_ _{ client = cli_ }
          H.liftAff $ saveForm form
          H.raise $ Authenticated cli_

        Left s -> do
          H.modify_ _{ client = cli }
          H.raise $ Failed cli form s

  Unauthenticate -> do
    Api.Client { endpoint } <- H.gets _.client
    let cli = Api.makeClient endpoint
    H.modify_ _{ client = cli }
    H.raise $ Unauthenticated $ cli

  SetString attr v -> do
    assign (_form <<< attr) v

  SetCode code -> do
    Api.AuthenticateForm form <- H.gets _.form
    H.modify_ $ _{ form = Api.AuthenticateForm $ form { code = code } }

  SetTel tel -> do
    Api.AuthenticateForm form <- H.gets _.form
    H.modify_ $ _{ form = Api.AuthenticateForm $ form { tel = tel } }


loadForm :: Aff Api.AuthenticateForm
loadForm = do
  code <- Storage.get "user.code"
  tel <- Storage.get "user.tel"
  pure $ Api.AuthenticateForm { code, tel }

saveForm :: Api.AuthenticateForm -> Aff Unit
saveForm (Api.AuthenticateForm { code, tel }) = do
  Storage.set "user.code" code
  Storage.set "user.tel" tel
