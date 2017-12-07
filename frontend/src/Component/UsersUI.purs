module Component.UsersUI where

import Prelude

import Api.Token (AuthenticationToken)
import Api.Users as Users
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (ExceptT, lift, runExcept, runExceptT, throwError)
import Control.Monad.State (class MonadState)
import Data.DateTime as DateTime
import Data.DateTime.Locale (Locale(Locale))
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Foreign (Foreign)
import Data.Foreign.Class (decode)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Model.User (User(..), Users)
import Network.HTTP.Affjax (AJAX, URL)


data Query a
  = SetLocale Locale a
  | SetToken AuthenticationToken a
  | Scan a

type State =
  { items :: Users
  , token :: Maybe AuthenticationToken
  , baseUrl :: URL
  , locale :: Locale
  , alerts :: Array String
  , busy :: Boolean
  }

type Input =
  { locale :: Locale
  , baseUrl :: URL
  }

data Message
  = Failed String


type Eff_ eff = Aff (ajax :: AJAX | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.component
    { initialState: initialState
    , render
    , eval
    , receiver: const Nothing
    }

initialState :: Input -> State
initialState { locale, baseUrl } =
    { items: []
    , token: Nothing
    , baseUrl: baseUrl
    , alerts: []
    , busy: false
    , locale: locale
    }

render :: State -> H.ComponentHTML Query
render state =
  HH.div_
  [ HH.div
    [ HP.class_ $ H.ClassName "row" ]
    (renderItem <$> state.items)
  , HH.div_ (renderAlert <$> state.alerts)
  , renderBusy state.busy
  ]

  where
    renderAlert s =
      HH.pre_
      [ HH.text s ]

    renderBusy false = HH.p_ []
    renderBusy true =
      HH.p
      [ HP.class_ $ H.ClassName "text-center" ]
      [
        HH.i [ HP.class_ $ H.ClassName "fa fa-spinner fa-pulse fa-3x" ] []
      ]

    renderItem (User { id, code, tel, name, created_at }) =
      HH.div
      [ HP.classes [ H.ClassName "col-md-12" ] ]
      [ HH.div
        [ HP.classes [ H.ClassName "card", H.ClassName "mb-2" ] ]
        [
          HH.div
          [ HP.classes [ H.ClassName "card-body" ] ]
          [ HH.p
            [ HP.classes [ H.ClassName "card-text small" ] ]
            [
              HH.i
              [ HP.class_ $ H.ClassName "fa fa-user"
              , HP.title name
              ] []
            , HH.text name
            ]
          -- , HH.p
          --   [ HP.classes [ H.ClassName "card-text small" ] ]
          --   [
          --     HH.text $ created_at
          --   ]
          , HH.p
            [ HP.classes [ H.ClassName "card-text", H.ClassName "text-muted", H.ClassName "small" ] ]
            [ renderDateTime created_at state.locale ]
          ]
        ]
      ]

    renderDateTime dt (Locale _ dur) =
      HH.text $ either id id $ maybe (Left "") (formatDateTime "YYYY/MM/DD HH:mm:ss") dt_
      where
        dt_ = (DateTime.adjust (negate dur)) dt

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
  SetLocale locale next -> do
    H.modify _{ locale = locale }
    pure next

  SetToken token next -> do
    H.modify _{ token = Just token }
    pure next

  Scan next -> do
    url <- H.gets _.baseUrl
    res <- runExceptT do
      token <- verifyToken
      users <- onLeft "Failed to access api" =<< (H.liftAff $ attempt $ Users.index url token)
      lift do
        H.modify _{ items = users }

    either (H.raise <<< Failed) pure res

    pure next

onNothing :: forall m. Monad m => String -> Maybe ~> ExceptT String m
onNothing s = maybe (throwError s) pure

verifyToken :: forall m. MonadState State m => ExceptT String m AuthenticationToken
verifyToken =
  onNothing "Token is not ready" =<< (lift $ H.gets _.token)

onLeft :: forall e m. Monad m => String -> Either e ~> ExceptT String m
onLeft s = either (throwError <<< const s) pure

getItems :: forall eff. Array Foreign -> Aff eff (Array User)
getItems objs =
  case runExcept (traverse decode objs) of
    Right xs -> pure xs
    Left err ->
      throwError $ (error <<< intercalate "\n\n" <<< map show) err
