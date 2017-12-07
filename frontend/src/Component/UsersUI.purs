module Component.UsersUI where

import Prelude

import Api.Token (AuthenticationToken)
import Api.Users as Users
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
derive instance eqUsersSlot :: Eq Slot
derive instance ordUsersSlot :: Ord Slot


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
  , HH.div
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
