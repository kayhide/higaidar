module Component.UserListUI where

import Prelude

import Api as Api
import Api.Users as Users
import Component.HTML.LoadingIndicator as LoadingIndicator
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Data.DateTime.Locale (Locale)
import Data.Either (Either, either)
import Data.Maybe (Maybe(Nothing, Just))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import I18n as I18n
import Model.User (User(..), Users)
import Network.HTTP.Affjax (AJAX)
import Route as R


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


data Query a
  = Initialize a
  | SetLocale Locale a
  | Reload a

type State =
  { items :: Users
  , client :: Api.Client
  , locale :: Locale
  , busy :: Boolean
  }

type Input =
  { client :: Api.Client
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
initialState { client, locale } =
    { items: []
    , client
    , locale
    , busy: false
    }

render :: State -> H.ComponentHTML Query
render state =
  HH.div_
  [
    HH.h1_
    [ HH.text "User List" ]
  , LoadingIndicator.render state.busy
  , HH.table
    [ HP.class_ $ H.ClassName "table table-striped table-bordered table-hover table-sm" ]
    [
      HH.thead_
      [ renderHeader ]
    , HH.tbody_
      $ renderItem <$> state.items
    ]
  ]

  where
    renderHeader =
      HH.tr_
      [
        HH.td_
        [ HH.text "Name" ]
      , HH.td_
        [ HH.text "Code" ]
      , HH.td_
        [ HH.text "Tel" ]
      , HH.td_
        [ HH.text "Created at" ]
      ]

    renderItem (User { id, code, tel, name, created_at }) =
      HH.tr_
      [
        HH.td_
        [
          HH.a
           [ HP.href $ R.path $ R.UsersShow id ]
           [ HH.text name ]
        ]
      , HH.td_
        [ HH.text $ show code ]
      , HH.td_
        [ HH.text tel ]
      , HH.td_
        [ HH.text $ I18n.localizeDateTime state.locale created_at ]
      ]

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
      cli <- H.gets _.client
      res <- runExceptT do
        users <- onLeft "Failed to access api" =<< (H.liftAff $ attempt $ Users.index cli)
        lift do
          H.modify _{ items = users }

      either (H.raise <<< Failed) pure res

    H.modify _{ busy = false }
    pure next

onLeft :: forall e m. Monad m => String -> Either e ~> ExceptT String m
onLeft s = either (throwError <<< const s) pure
