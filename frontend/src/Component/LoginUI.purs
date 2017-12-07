module Component.LoginUI where

import Prelude

import Api.Token (UserCode, UserTel, AuthenticationToken)
import Api.Token as Token
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Maybe.Trans (lift)
import Data.Either (Either(..), either)
import Data.Int (fromString)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Tuple (Tuple(..))
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
  { baseUrl :: URL
  }

type State =
  { config :: Config
  , code :: UserCode
  , tel :: UserTel
  , token :: Maybe AuthenticationToken
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
  = Authenticated UserCode UserTel AuthenticationToken
  | Failed String


type Eff_ eff = Aff (ajax :: AJAX, storage :: STORAGE | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleComponent
    { initialState: { config: _
                    , code: 0
                    , tel: ""
                    , token: Nothing
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
render state =
  HH.form
  [ HP.class_ $ H.ClassName "form-inline my-2 my-lg-0" ]
  [
    HH.input
    [ HP.class_ $ H.ClassName "form-control mr-2"
    , HP.value $ show state.code
    , HE.onValueInput $ HE.input SetCode
    ]
  , HH.input
    [ HP.class_ $ H.ClassName "form-control mr-2"
    , HP.value state.tel
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
      "btn " <> maybe "btn-outline-secondary" (const "btn-secondary") state.token

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    user <- H.liftAff $ attempt do
      code <- Storage.get "user.code"
      tel <- Storage.get "user.tel"
      pure $ Tuple code tel

    case user of
      Right (Tuple code tel) ->
        next # (eval <<< SetCode code)
        >>= (eval <<< SetTel tel)
        >>= (eval <<< Authenticate)
      Left s ->
        pure next

  Authenticate next -> do
    url <- (_ <> "/token") <$> H.gets _.config.baseUrl
    code <- H.gets _.code
    tel <- H.gets _.tel
    res <- runExceptT do
      token <- onLeft "Authentication failed" =<< (H.liftAff $ attempt $ Token.authenticate url code tel)
      lift $ do
        H.modify _{ token = Just token }
        H.raise $ Authenticated code tel token

    either (H.raise <<< Failed) pure res

    pure next

  SetCode code next -> do
    case fromString code of
      Just code_ ->
        H.modify $ _{ code = code_ }
      Nothing ->
        pure unit
    pure next

  SetTel tel next -> do
    H.modify $ _{ tel = tel }
    pure next


onNothing :: forall m. Monad m => String -> Maybe ~> ExceptT String m
onNothing s = maybe (throwError s) pure

onLeft :: forall e m. Monad m => String -> Either e ~> ExceptT String m
onLeft s = either (throwError <<< const s) pure
