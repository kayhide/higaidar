module Component.LoginUI where

import Prelude

import Api as Api
import Api.Token as Token
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Either (Either(..), either)
import Data.Int (fromString)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, maybe)
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
  }

type State =
  { config :: Config
  , form :: Api.AuthenticateForm
  , authenticated :: Boolean
  , name :: Maybe String
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
                    , authenticated: false
                    , name: Nothing
                    }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

render :: State -> H.ComponentHTML Query
render state@({ form: Api.AuthenticateForm { code, tel } }) =
  HH.form
  [ HP.class_ $ H.ClassName "form-inline my-2 my-lg-0" ]
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
    [ HP.class_ $ H.ClassName $ buttonClass
    , HE.onClick $ HE.input_ Authenticate
    ]
    [
      HH.i [ HP.class_ $ H.ClassName "fa fa-sign-in fa-fw mr-1" ] []
    , HH.text "Login"
    ]
  ]

  where
    buttonClass =
      "btn " <> if state.authenticated then "btn-secondary" else "btn-outline-secondary"

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    form <- H.liftAff $ attempt loadForm
    case form of
      Right form_ -> do
        H.modify _{ form = form_ }
        eval $ Authenticate next
      Left s ->
        pure next

  Authenticate next -> do
    endpoint <- H.gets _.config.endpoint
    form <- H.gets _.form
    let cli = Api.makeClient endpoint

    res <- runExceptT do
      onLeft "Authentication failed"
        =<< (H.liftAff $ attempt $ Token.authenticate cli form)

    case res of
      Right cli_ -> do
        H.modify _{ authenticated = true }
        H.liftAff $ saveForm form
        H.raise $ Authenticated cli_

      Left s -> do
        H.modify _{ authenticated = false }
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


onNothing :: forall m. Monad m => String -> Maybe ~> ExceptT String m
onNothing s = maybe (throwError s) pure

onLeft :: forall e m. Monad m => String -> Either e ~> ExceptT String m
onLeft s = either (throwError <<< const s) pure

loadForm :: forall eff. Aff (storage :: STORAGE | eff) Api.AuthenticateForm
loadForm = do
  code <- (fromMaybe 0 <<< fromString) <$> Storage.get "user.code"
  tel <- Storage.get "user.tel"
  pure $ Api.AuthenticateForm { code, tel }

saveForm :: forall eff. Api.AuthenticateForm -> Aff (storage :: STORAGE | eff) Unit
saveForm (Api.AuthenticateForm { code, tel }) = do
  Storage.set "user.code" $ show code
  Storage.set "user.tel" tel
