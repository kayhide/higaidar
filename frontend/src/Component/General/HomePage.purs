module Component.General.HomePage where

import Prelude

import Api as Api
import Api.My.Photos as Photos
import Api.Pests as Pests
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.UploadUI as UploadUI
import Component.Util as Util
import Control.Monad.Aff (Aff, Milliseconds(..), attempt, delay)
import Data.Array as Array
import Data.DateTime.Locale (Locale)
import Data.Either (Either(Left, Right))
import Data.Lens (view, (.~))
import Data.Maybe (Maybe(Just, Nothing), isJust, isNothing, maybe)
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HM
import I18n.Ja as Ja
import Model.Pest (Pest(..))
import Model.Photo (Photo(..), PhotoId, _original_url, _pest, _thumbnail_url)
import Model.Photo as Photo
import Network.HTTP.Affjax (AJAX, URL)


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


data Query a
  = Initialize a
  | Reload a
  | SetPest Photo String a
  | Destroy PhotoId a
  | ToggleDeleting a
  | HandleUpload UploadUI.Message a

type State =
  { items :: Array Photo
  , loadingItems :: Array URL
  , client :: Api.Client
  , locale :: Locale
  , pests :: Array Pest
  , deleting :: Boolean
  , busy :: Boolean
  }

type Input =
  { client :: Api.Client
  , locale :: Locale
  }

data Message
  = Failed String

type ChildQuery = UploadUI.Query
type ChildSlot = UploadUI.Slot

type Eff_ eff = Aff (ajax :: AJAX | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where
    initialState { client, locale } =
      { items: []
      , loadingItems: []
      , client
      , locale
      , pests: []
      , deleting: false
      , busy: false
      }

render :: forall eff. State -> H.ParentHTML Query ChildQuery ChildSlot (Eff_ eff)
render state =
  HH.div_
  [
    renderDeletingButton
  , HH.h1_
    [ HH.text Ja.photo_list ]
  , LoadingIndicator.render state.busy
  , HH.div
    [ HP.class_ $ H.ClassName "row no-gutters" ]
    $ [ renderUploadUI ]
    <> (renderLoadingItem <$> state.loadingItems)
    <> (renderItem <$> state.items)
  ]

  where
    client = state.client
    deleting = state.deleting

    renderDeletingButton =
      HH.div
      [ HP.class_ $ H.ClassName "pull-right mt-2" ]
      [
        HH.button
        [ HP.class_ $ H.ClassName $ "btn rounded-circle " <> if deleting then "btn-danger" else "btn-outline-danger"
        , HE.onClick $ HE.input_ $ ToggleDeleting
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fa fa-trash" ] []
        ]
      ]

    renderUploadUI =
      HH.div
      [ HP.class_ $ H.ClassName "col-md-2 col-sm-4 col-6 pb-2" ]
      [ HH.div
        [ HP.class_ $ H.ClassName "card" ]
        [
          HH.div
          [ HP.class_ $ H.ClassName "card-body text-center" ]
          [
            HH.slot UploadUI.Slot UploadUI.ui { client } $ HE.input HandleUpload
          ]
        ]
      ]

    renderLoadingItem url =
      HH.div
      [ HP.class_ $ H.ClassName "col-md-2 col-sm-4 col-6 pb-2" ]
      [ HH.div
        [ HP.class_ $ H.ClassName "card" ]
        [
          HH.div
          [ HP.class_ $ H.ClassName "card-body text-center" ]
          [
            HH.i [ HP.class_ $ H.ClassName "fa fa-spinner fa-pulse fa-3x" ] []
          ]
        ]
      ]

    renderItem photo@(Photo { id, original_url, thumbnail_url, pest, created_at }) =
      HH.div
      [ HP.class_ $ H.ClassName "col-md-2 col-sm-4 col-6 pb-2" ]
      [ HH.div
        [ HP.class_ $ H.ClassName "card position-relative" ]
        [
          HH.a
          [ HP.href original_url, HP.target "_blank" ]
          [
            maybe (HH.div_ []) renderThumbnail thumbnail_url
          ]
        , renderDeleteButton id
        , renderPest photo
        ]
      ]

    renderThumbnail url =
      HH.img
      [ HP.src url
      , HP.class_ $ H.ClassName "card-img-top"
      ]

    renderDeleteButton id =
      HH.div
      [ HP.class_ $ H.ClassName $ "delete-photo-button" <> if deleting then " _on" else "" ]
      [
        HH.div
        [ HP.class_ $ H.ClassName "card-body text-center" ]
        [
          HH.button
          [ HP.class_ $ H.ClassName "btn btn-danger rounded-circle"
          , HE.onClick $ HE.input_ $ Destroy id
          ]
          [ HH.i [ HP.class_ $ H.ClassName "fa fa-times" ] []
          ]
        ]
      ]

    renderPest photo@(Photo { pest }) =
      HH.select
      [ HP.class_ $ H.ClassName "form-control"
      , HE.onValueChange $ HE.input $ SetPest photo
      ]
      $ [ renderDefaultPestOption pest ]
      <> (renderPestOption pest <$> state.pests)

    renderDefaultPestOption selected =
      HH.option
      [ HP.enabled false
      , HP.selected $ isNothing selected
      ]
      [ HH.text Ja.select_pest ]

    renderPestOption selected (Pest { label }) =
      HH.option
      [ HP.value $ label
      , HP.selected $ selected == Just label
      ]
      [ HH.text label ]

eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    void $ HM.fork loadPests
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

  SetPest photo label next -> do
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      let pest = if String.null label then Nothing else Just label
      res <- H.liftAff $ attempt $ Photos.update cli $ photo # _pest .~ pest

      case res of
        Right _ -> do
          pure unit
        Left _ -> do
          H.raise $ Failed "Failed to update photo."

    pure next

  Destroy photoId next -> do
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      res <- H.liftAff $ attempt $ Photos.destroy cli photoId

      case res of
        Right _ -> do
          items <- Array.filter ((photoId /= _) <<< view Photo._id) <$> H.gets _.items
          H.modify _{ items = items }
        Left _ ->
          H.raise $ Failed "Failed to destroy photo."

    pure next

  ToggleDeleting next -> do
    deleting <- not <$> H.gets _.deleting
    H.modify _{ deleting = deleting }
    pure next

  HandleUpload (UploadUI.Uploaded url) next -> do
    loadingItems <- Array.cons url <$> H.gets _.loadingItems
    H.modify _{ loadingItems = loadingItems }
    pure next

  HandleUpload (UploadUI.Failed s) next -> do
    H.raise $ Failed s
    pure next

loadPests :: forall eff. H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff) Unit
loadPests = do
  cli <- H.gets _.client
  res <- H.liftAff $ attempt $ Pests.index cli

  case res of
    Right pests -> do
      H.modify _{ pests = pests }
    Left _ -> do
      H.raise $ Failed "Failed to load Pests. Retrying..."
      H.liftAff $ delay (Milliseconds 5000.0)
      loadPests

runPoller :: forall eff. H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff) Unit
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
