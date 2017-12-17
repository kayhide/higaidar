module Component.MyPhotoListUI where

import Prelude

import Api as Api
import Api.My.Photos as Photos
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.Util as Util
import Control.Monad.Aff (Aff, Milliseconds(..), attempt, delay)
import Data.Array as Array
import Data.DateTime as DateTime
import Data.DateTime.Locale (Locale(..))
import Data.Either (Either(Left, Right), either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Lens (view)
import Data.Maybe (Maybe(Nothing, Just), isJust, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HM
import Model.Photo (Photo(..), PhotoId, _original_url, _thumbnail_url)
import Model.Photo as Photo
import Network.HTTP.Affjax (AJAX, URL)


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


data Query a
  = Initialize a
  | Reload a
  | Destroy PhotoId a
  | PushLoadingItem URL a

type State =
  { items :: Array Photo
  , loadingItems :: Array URL
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
    , loadingItems: []
    , client
    , locale
    , busy: false
    }

render :: State -> H.ComponentHTML Query
render state =
  HH.div_
  [
    HH.h1_
    [ HH.text "Photo List" ]
  , LoadingIndicator.render state.busy
  , HH.div
    [ HP.class_ $ H.ClassName "row no-gutters" ]
    $ (renderLoadingItem <$> state.loadingItems) <> (renderItem <$> state.items)
  ]

  where
    renderLoadingItem url =
      HH.div
      [ HP.classes [ H.ClassName "col-md-2", H.ClassName "col-sm-4", H.ClassName "col-6" ] ]
      [ HH.div
        [ HP.classes [ H.ClassName "card h-100" ] ]
        [
          HH.div
          [ HP.classes [ H.ClassName "card-body text-center" ] ]
          [
            HH.i [ HP.class_ $ H.ClassName "fa fa-spinner fa-pulse fa-3x" ] []
          ]
        ]
      ]

    renderItem (Photo { id, original_url, thumbnail_url, created_at }) =
      HH.div
      [ HP.classes [ H.ClassName "col-md-2", H.ClassName "col-sm-4", H.ClassName "col-6" ] ]
      [ HH.div
        [ HP.classes [ H.ClassName "card h-100" ] ]
        [
          HH.a
          [ HP.href original_url, HP.target "_blank" ]
          [
            renderThumbnail thumbnail_url
          ]
        , HH.div
          [ HP.classes [ H.ClassName "card-body" ] ]
          [
            HH.p
            [ HP.classes [ H.ClassName "card-text", H.ClassName "text-muted", H.ClassName "small" ] ]
            [ renderDateTime created_at state.locale ]
          ]
        ]
      ]

    renderThumbnail = case _ of
      Just url ->
        HH.img
        [ HP.src url
        , HP.class_ $ H.ClassName "card-img-top"
        ]
      Nothing ->
        HH.div_ []

    renderDateTime dt (Locale _ dur) =
      HH.text $ either id id $ maybe (Left "") (formatDateTime "YYYY/MM/DD HH:mm:ss") dt_
      where
        dt_ = (DateTime.adjust (negate dur)) dt

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    void $ HM.fork runPoller
    eval $ Reload next

  Reload next -> do
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      photos <- H.liftAff $ attempt $ Photos.index cli

      case photos of
        Right photos_ ->
          H.modify _{ items = photos_ }
        Left _ ->
          H.raise $ Failed "Failed to access api."

    pure next

  Destroy userId next -> do
    busy <- H.gets _.busy
    when (not busy) do
      H.modify _{ busy = true }
      cli <- H.gets _.client
      res <- H.liftAff $ attempt $ Photos.destroy cli userId

      case res of
        Right _ -> do
          items <- Array.filter ((userId /= _) <<< view Photo._id) <$> H.gets _.items
          H.modify _{ items = items }
        Left _ ->
          H.raise $ Failed "Failed to delete user."

      H.modify _{ busy = false }
    pure next

  PushLoadingItem url next -> do
    loadingItems <- Array.cons url <$> H.gets _.loadingItems
    H.modify _{ loadingItems = loadingItems }
    pure next


runPoller :: forall eff. H.ComponentDSL State Query Message (Eff_ eff) Unit
runPoller = do
  H.liftAff $ delay (Milliseconds 1000.0)
  loadingItems <- H.gets _.loadingItems
  case loadingItems of
    [] -> pure unit
    urls -> do
      cli <- H.gets _.client
      photos <- H.liftAff $ attempt $ Photos.index cli

      case photos of
        Right photos_ -> do
          let photos__ =
                Array.filter (isJust <<< view _thumbnail_url)
                $ Array.filter (flip Array.elem urls <<< view _original_url) photos_
              loadingItems_ = Array.difference loadingItems $ view _original_url <$> photos__

          items <- append photos__ <$> H.gets _.items
          H.modify _{ items = items, loadingItems = loadingItems_ }
        Left _ ->
          pure unit
  runPoller
