module Component.General.HomePage where

import AppPrelude

import Affjax (URL)
import Api as Api
import Api.My.Photos as Photos
import Api.Pests as Pests
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.UploadUI as UploadUI
import Component.Util as Util
import Control.Monad.State (class MonadState)
import Data.Array as Array
import Data.Lens (view)
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HM
import I18n.Ja as Ja
import Model.DateTime (Locale)
import Model.Pest (Pest (..))
import Model.Photo (Photo (..), PhotoId, _original_url, _pest, _thumbnail_url)
import Model.Photo as Photo


data Action
  = Initialize
  | Reload
  | SetPest Photo String
  | Destroy PhotoId
  | ToggleDeleting
  | HandleUpload UploadUI.Message

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

type ChildSlots =
  ( upload :: H.Slot (Const Void) UploadUI.Message Unit
  )


ui ::
  forall m.
  MonadAff m =>
  H.Component HH.HTML (Const Void) Input Message m
ui =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    }
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

render ::
  forall m.
  MonadAff m =>
  State -> H.ComponentHTML Action ChildSlots m
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
        , HE.onClick $ const $ Just $ ToggleDeleting
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
            HH.slot (SProxy :: _ "upload") unit UploadUI.ui { client } $ Just <<< HandleUpload
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
          , HE.onClick $ const $ Just $ Destroy id
          ]
          [ HH.i [ HP.class_ $ H.ClassName "fa fa-times" ] []
          ]
        ]
      ]

    renderPest photo@(Photo { pest }) =
      HH.select
      [ HP.class_ $ H.ClassName "form-control"
      , HE.onValueChange $ Just <<< SetPest photo
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

handleAction ::
  forall m.
  MonadAff m =>
  Action -> H.HalogenM State Action ChildSlots Message m Unit
handleAction = case _ of
  Initialize -> do
    void $ HM.fork loadPests
    void $ HM.fork runPoller
    handleAction $ Reload

  Reload -> do
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      photos <- H.liftAff $ attempt $ Photos.index cli

      case photos of
        Right photos_ ->
          H.modify_ _{ items = photos_ }
        Left _ ->
          H.raise $ Failed "Failed to access api."

  SetPest photo label -> do
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      let pest = if String.null label then Nothing else Just label
      res <- H.liftAff $ attempt $ Photos.update cli $ photo # _pest .~ pest

      case res of
        Right _ -> do
          pure unit
        Left _ -> do
          H.raise $ Failed "Failed to update photo."

  Destroy photoId -> do
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      res <- H.liftAff $ attempt $ Photos.destroy cli photoId

      case res of
        Right _ -> do
          items <- Array.filter ((photoId /= _) <<< view Photo._id) <$> H.gets _.items
          H.modify_ _{ items = items }
        Left _ ->
          H.raise $ Failed "Failed to destroy photo."

  ToggleDeleting -> do
    deleting <- not <$> H.gets _.deleting
    H.modify_ _{ deleting = deleting }

  HandleUpload (UploadUI.Uploaded url) -> do
    loadingItems <- Array.cons url <$> H.gets _.loadingItems
    H.modify_ _{ loadingItems = loadingItems }

  HandleUpload (UploadUI.Failed s) -> do
    H.raise $ Failed s

loadPests :: forall m. MonadAff m => H.HalogenM State Action ChildSlots Message m Unit
loadPests = do
  cli <- H.gets _.client
  res <- H.liftAff $ attempt $ Pests.index cli

  case res of
    Right pests -> do
      H.modify_ _{ pests = pests }
    Left _ -> do
      H.raise $ Failed "Failed to load Pests. Retrying..."
      H.liftAff $ delay (Milliseconds 5000.0)
      loadPests

runPoller :: forall m. MonadAff m => MonadState State m => m Unit
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
          H.modify_ _{ items = items, loadingItems = loadingItems_ }
        Left _ ->
          pure unit
  runPoller
