module Component.UserListUI where

import Prelude

import Api.Token (AuthenticationToken)
import Api.Users as Users
import Component.HTML.LoadingIndicator as LoadingIndicator
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Data.DateTime as DateTime
import Data.DateTime.Locale (Locale(Locale))
import Data.Either (Either(..), either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Model.User (User(..), Users)
import Network.HTTP.Affjax (AJAX, URL)
import Route as R


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


data Query a
  = Initialize a
  | SetLocale Locale a
  | Scan a

type State =
  { items :: Users
  , baseUrl :: URL
  , token :: AuthenticationToken
  , locale :: Locale
  , alerts :: Array String
  , busy :: Boolean
  }

type Input =
  { baseUrl :: URL
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
initialState { locale, token, baseUrl } =
    { items: []
    , baseUrl
    , token
    , locale
    , alerts: []
    , busy: false
    }

render :: State -> H.ComponentHTML Query
render state =
  HH.div_
  [
    HH.h1_
    [ HH.text "User List" ]
  , LoadingIndicator.render state.busy
  , HH.div
    [ HP.class_ $ H.ClassName "row" ]
    (renderItem <$> state.items)
  , HH.div_ (renderAlert <$> state.alerts)
  ]

  where
    renderAlert s =
      HH.pre_
      [ HH.text s ]

    renderItem (User { id, code, tel, name, created_at }) =
      HH.div
      [ HP.class_ $ H.ClassName "col-md-12" ]
      [ HH.div
        [ HP.class_ $ H.ClassName "card mb-2" ]
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
          -- , HH.p
          --   [ HP.class_ [ H.ClassName "card-text small" ] ]
          --   [
          --     HH.text $ created_at
          --   ]
          , HH.p
            [ HP.class_ $ H.ClassName "card-text text-muted small" ]
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
  Initialize next -> do
    eval $ Scan next

  SetLocale locale next -> do
    H.modify _{ locale = locale }
    pure next

  Scan next -> do
    busy <- H.gets _.busy
    when (not busy) do
      H.modify _{ busy = true }
      url <- H.gets _.baseUrl
      token <- H.gets _.token
      res <- runExceptT do
        users <- onLeft "Failed to access api" =<< (H.liftAff $ attempt $ Users.index url token)
        lift do
          H.modify _{ items = users }

      either (H.raise <<< Failed) pure res

    H.modify _{ busy = false }
    pure next

onLeft :: forall e m. Monad m => String -> Either e ~> ExceptT String m
onLeft s = either (throwError <<< const s) pure
