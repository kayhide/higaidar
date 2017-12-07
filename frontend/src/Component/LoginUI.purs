module Component.LoginUI where

import Prelude

import Api.Token (UserCode, UserTel, AuthenticationToken)
import Api.Token as Token
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Maybe.Trans (lift)
import Data.Either (Either, either)
import Data.Int (fromString)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX, URL)


type LoginConfig =
  { baseUrl :: URL
  }

type State =
  { config :: LoginConfig
  , code :: UserCode
  , tel :: UserTel
  , token :: Maybe AuthenticationToken
  , authenticated :: Boolean
  , name :: Maybe String
  }

data Query a
  = Authenticate a
  | SetCode String a
  | SetTel String a

type Input = LoginConfig

data Message
  = Authenticated UserCode UserTel AuthenticationToken
  | Failed String


type Eff_ eff = Aff (ajax :: AJAX | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.component
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
    }

render :: State -> H.ComponentHTML Query
render state =
  HH.div
  [ HP.class_ $ H.ClassName "form-inline my-2 my-lg-0" ]
  [
    HH.span
    [ HP.class_ $ H.ClassName "navbar-text mr-2" ]
    [ HH.text $ show $ state.code ]
  , HH.input
    [ HP.class_ $ H.ClassName "input"
    , HP.value $ show state.code
    , HE.onValueInput $ HE.input SetCode
    ]
  , HH.input
    [ HP.class_ $ H.ClassName "input"
    , HP.value state.tel
    , HE.onValueInput $ HE.input SetTel
    ]
  , HH.button
    [ HP.class_ $ H.ClassName $ buttonClass
    , HE.onClick $ HE.input_ Authenticate
    ]
    [
      HH.i [ HP.class_ $ H.ClassName "fa fa-user fa-fw" ] []
    ]
  ]

  where
    buttonClass =
      case state.token of
        Just _ -> "btn btn-sm btn-secondary"
        Nothing -> "btn btn-sm btn-outline-secondary"

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
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
