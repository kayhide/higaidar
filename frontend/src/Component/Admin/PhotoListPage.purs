module Component.Admin.PhotoListPage where

import Prelude

import Api as Api
import Api.Photos as Photos
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.UploadUI as UploadUI
import Component.Util as Util
import Control.Monad.Aff (Aff, attempt)
import Data.Array as Array
import Data.DateTime.Locale (Locale)
import Data.Either (Either(Left, Right))
import Data.Lens (view)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n.Ja as Ja
import Model.Photo (Photo(Photo), PhotoId)
import Model.Photo as Photo
import Network.HTTP.Affjax (AJAX)


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


data Query a
  = Initialize a
  | Reload a
  | Destroy PhotoId a
  | ToggleDeleting a

type State =
  { items :: Array Photo
  , client :: Api.Client
  , locale :: Locale
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
      , client
      , locale
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
    $ renderItem <$> state.items
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
        , HH.div
          [ HP.class_ $ H.ClassName "card-body" ]
          [
            HH.text $ show id
          , renderPest pest
          ]
        ]
      ]

    renderThumbnail url =
      HH.img
      [ HP.src url
      , HP.class_ $ H.ClassName "card-img-top"
      ]

    renderPest = case _ of
      Just pest ->
        HH.div_
        [ HH.text pest ]
      Nothing ->
        HH.div
        [ HP.class_ $ H.ClassName "text-muted" ]
        [ HH.text Ja.no_pest ]


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

eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
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
