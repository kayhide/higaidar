module Component.LoginUI where

import Prelude

import Api as Api
import Api.Token as Token
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.Util as Util
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(Left, Right))
import Data.Int (fromString)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Dom.Storage (STORAGE)
import Dom.Storage as Storage
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX, URL)


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


type Config =
  { endpoint :: URL
  , isAuthenticated :: Boolean
  }

type State =
  { config :: Config
  , form :: Api.AuthenticateForm
  , isAuthenticated :: Boolean
  , name :: Maybe String
  , busy :: Boolean
  }

data Query a
  = Initialize a
  | Authenticate a
  | SetCode String a
  | SetTel String a

type Input = Config

data Message
  = Authenticated Api.Client
  | Failed Api.Client Api.AuthenticateForm String


type Eff_ eff = Aff (ajax :: AJAX, storage :: STORAGE | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleComponent
    { initialState: { config: _
                    , form: Api.AuthenticateForm { code: 0, tel: "" }
                    , isAuthenticated: false
                    , name: Nothing
                    , busy: false
                    }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

render :: State -> H.ComponentHTML Query
render state@({ form: Api.AuthenticateForm { code, tel } }) =
  HH.div_
  [
    HH.h1_
    [ HH.text "Login" ]
  , LoadingIndicator.render state.busy
  , renderForm
  ]

  where
    renderForm =
      HH.div
      [ HP.class_ $ H.ClassName "form" ]
      [
        HH.input
        [ HP.class_ $ H.ClassName "form-control mr-2"
        , HP.value $ show code
        , HE.onValueInput $ HE.input SetCode
        ]
      , HH.input
        [ HP.class_ $ H.ClassName "form-control mr-2"
        , HP.value tel
        , HE.onValueInput $ HE.input SetTel
        ]
      , HH.button
        [ HP.class_ $ H.ClassName buttonClass
        , HE.onClick $ HE.input_ Authenticate
        ]
        [
          HH.i [ HP.class_ $ H.ClassName "fa fa-sign-in fa-fw mr-1" ] []
        , HH.text "Login"
        ]
      ]

    buttonClass =
      "btn " <> if state.isAuthenticated then "btn-secondary" else "btn-outline-secondary"

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    form <- H.liftAff $ attempt loadForm
    case form of
      Right form_ -> do
        isAuthenticated <-  H.gets _.config.isAuthenticated
        H.modify _{ form = form_ }
        H.gets _.config.isAuthenticated >>= case _ of
          true -> pure next
          false -> eval $ Authenticate next
      Left s ->
        pure next

  Authenticate next -> do
    Util.whenNotBusy_ do
      endpoint <- H.gets _.config.endpoint
      form <- H.gets _.form
      let cli = Api.makeClient endpoint

      res <- runExceptT do
        Util.onLeft "Authentication failed"
          =<< (H.liftAff $ attempt $ Token.authenticate cli form)

      case res of
        Right cli_ -> do
          H.modify _{ isAuthenticated = true }
          H.liftAff $ saveForm form
          H.raise $ Authenticated cli_

        Left s -> do
          H.modify _{ isAuthenticated = false }
          H.raise $ Failed cli form s

    pure next

  SetCode code next -> do
    case fromString code of
      Just code_ -> do
        (Api.AuthenticateForm form) <- H.gets _.form
        H.modify $ _{ form = Api.AuthenticateForm $ form { code = code_ } }
      Nothing ->
        pure unit
    pure next

  SetTel tel next -> do
    (Api.AuthenticateForm form) <- H.gets _.form
    H.modify $ _{ form = Api.AuthenticateForm $ form { tel = tel } }
    pure next


loadForm :: forall eff. Aff (storage :: STORAGE | eff) Api.AuthenticateForm
loadForm = do
  code <- (fromMaybe 0 <<< fromString) <$> Storage.get "user.code"
  tel <- Storage.get "user.tel"
  pure $ Api.AuthenticateForm { code, tel }

saveForm :: forall eff. Api.AuthenticateForm -> Aff (storage :: STORAGE | eff) Unit
saveForm (Api.AuthenticateForm { code, tel }) = do
  Storage.set "user.code" $ show code
  Storage.set "user.tel" tel
