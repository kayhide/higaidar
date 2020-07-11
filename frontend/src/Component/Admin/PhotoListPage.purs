module Component.Admin.PhotoListPage where

import AppPrelude

import Api as Api
import Api.Photos as Photos
import Api.Users as Users
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.UploadUI as UploadUI
import Component.Util as Util
import Control.Monad.State (execState)
import Data.Array as Array
import Data.Lens (assign, view)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Map (Map)
import Data.Map as Map
import Data.Profunctor.Strong ((&&&))
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HM
import I18n.Ja as Ja
import Model.DateTime (Locale)
import Model.Photo (Photo (..), PhotoId)
import Model.Photo as Photo
import Model.User (User(..), UserId)
import Model.User as User


data Action
  = Reload
  | Destroy PhotoId
  | ToggleDeleting
  | SelectUser UserId
  | UnselectUser UserId
  | SelectPest String
  | UnselectPest String

type State =
  { items :: Array Photo
  , users :: Map UserId User
  , client :: Api.Client
  , locale :: Locale
  , deleting :: Boolean
  , busy :: Boolean
  , selectedUsers :: Array UserId
  , selectedPests :: Array String
  }

type Input =
  { client :: Api.Client
  , locale :: Locale
  }

type Query = Const Void

data Message
  = Failed String

type ChildSlots =
  ( upload :: H.Slot UploadUI.Query UploadUI.Message Unit
  )

ui ::
  forall m.
  MonadAff m =>
  H.Component HH.HTML Query Input Message m
ui =
  H.mkComponent
  { initialState
  , render
  , eval:
    H.mkEval
    $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Reload
    }
  }

  where
    initialState { client, locale } =
      { items: []
      , users: Map.empty
      , client
      , locale
      , deleting: false
      , busy: false
      , selectedUsers: []
      , selectedPests: []
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
    [ HP.class_ $ H.ClassName "mb-2" ]
    [
      HH.div_
      $ (renderFilteringUser <$> state.selectedUsers)
      <> (renderFilteringPest <$> state.selectedPests)
    ]
  , HH.div
    [ HP.class_ $ H.ClassName "row no-gutters" ]
    $ renderItem <$> state.items
  ]

  where
    client = state.client
    deleting = state.deleting

    renderFilteringUser userId =
      HH.div
      [ HP.class_ $ H.ClassName "btn-group mr-2" ]
      [
        HH.span
        [ HP.class_ $ H.ClassName "input-group-addon" ]
        [
          HH.i [ HP.class_ $ H.ClassName "fa fa-user mr-2" ] []
        , HH.text $ fromMaybe "..." $ state.users ^? (ix userId <<< User._name)
        ]
      , HH.a
        [ HP.class_ $ H.ClassName "btn btn-secondary btn-outline"
        , HP.href $ "#/photos"
        , HE.onClick $ const $ Just $ UnselectUser userId
        ]
        [
          HH.i [ HP.class_ $ H.ClassName "fa fa-times" ] []
        ]
      ]

    renderFilteringPest pest =
      HH.div
      [ HP.class_ $ H.ClassName "btn-group mr-2" ]
      [
        HH.span
        [ HP.class_ $ H.ClassName "input-group-addon" ]
        [
          HH.i [ HP.class_ $ H.ClassName "fa fa-bug mr-2" ] []
        , HH.text pest
        ]
      , HH.a
        [ HP.class_ $ H.ClassName "btn btn-secondary btn-outline"
        , HP.href $ "#/photos"
        , HE.onClick $ const $ Just $ UnselectPest pest
        ]
        [
          HH.i [ HP.class_ $ H.ClassName "fa fa-times" ] []
        ]
      ]

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

    renderItem photo@(Photo { id, user_id, original_url, thumbnail_url, pest, created_at }) =
      HH.div
      [ HP.class_ $ H.ClassName "col-md-2 col-sm-4 col-6 pb-2" ]
      [
        HH.div
        [ HP.class_ $ H.ClassName "card position-relative" ]
        [
          HH.a
          [ HP.href original_url, HP.target "_blank" ]
          [
            maybe (HH.div_ []) renderThumbnail thumbnail_url
          ]
        , renderDeleteButton id
        , HH.div
          [ HP.class_ $ H.ClassName "d-flex p-2 bg-light text-secondary" ]
          [
            HH.small_
            [ HH.text $ show id ]
          , renderUser $ Map.lookup user_id state.users
          ]
        , HH.div
          [ HP.class_ $ H.ClassName "card-body" ]
          [
            renderPest pest
          ]
        ]
      ]

    renderThumbnail url =
      HH.img
      [ HP.src url
      , HP.class_ $ H.ClassName "card-img-top"
      ]

    renderUser = case _ of
      Just (User { id, name }) ->
        HH.a
        [ HP.class_ $ H.ClassName "ml-auto small"
        , HP.href $ "#/photos?user_id=" <> show id
        , HE.onClick $ const $ Just $ SelectUser id
        ]
        [
          HH.i [ HP.class_ $ H.ClassName "fa fa-user mr-2" ] []
        , HH.text name
        ]
      Nothing ->
        HH.small
        [ HP.class_ $ H.ClassName "ml-auto text-muted" ]
        [ HH.text "..." ]

    renderPest = case _ of
      Just pest ->
        HH.a
        [ HP.class_ $ H.ClassName "ml-auto small"
        , HP.href $ "#/photos?pest=" <> pest
        , HE.onClick $ const $ Just $ SelectPest pest
        ]
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
          , HE.onClick $ const $ Just $ Destroy id
          ]
          [ HH.i [ HP.class_ $ H.ClassName "fa fa-times" ] []
          ]
        ]
      ]

handleAction ::
  forall m.
  MonadAff m =>
  Action -> H.HalogenM State Action ChildSlots Message m Unit
handleAction = case _ of
  Reload -> do
    Util.whenNotBusy_ do
      { client, selectedUsers, selectedPests } <- H.get
      let filters = flip execState Object.empty $ do
            when (not $ Array.null selectedUsers) $
              assign (at "user_id") $ Just $ Api.In_ $ show <$> selectedUsers
            when (not $ Array.null selectedPests) $
              assign (at "pest") $ Just $ Api.In_ selectedPests

      photos <- case Object.isEmpty filters of
        true ->
          H.liftAff $ attempt $ Photos.index client
        false ->
          H.liftAff $ attempt $ Photos.filter client filters

      case photos of
        Right photos_ -> do
          H.modify_ _{ items = photos_ }
          void $ HM.fork loadUsers
        Left _ ->
          H.raise $ Failed "Failed to access api."

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

  SelectUser userId -> do
    { selectedUsers } <- H.get
    if selectedUsers == [userId]
      then
        pure unit
      else do
        H.modify_ _{ selectedUsers = Array.nub $ Array.cons userId selectedUsers }
        handleAction Reload

  UnselectUser userId -> do
    { selectedUsers } <- H.get
    H.modify_ _{ selectedUsers = Array.delete userId selectedUsers }
    handleAction Reload

  SelectPest pest -> do
    { selectedPests } <- H.get
    if selectedPests == [pest]
      then
        pure unit
      else do
        H.modify_ _{ selectedPests = Array.nub $ Array.cons pest selectedPests }
        handleAction Reload

  UnselectPest pest -> do
    { selectedPests } <- H.get
    H.modify_ _{ selectedPests = Array.delete pest selectedPests }
    handleAction Reload

loadUsers :: forall m. MonadAff m => H.HalogenM State Action ChildSlots Message m Unit
loadUsers = do
  { items, users } <- H.get
  let ids = Array.difference (Array.nub $ view Photo._user_id <$> items) (Array.fromFoldable $ Map.keys users)

  when (not Array.null ids) $ do
    cli <- H.gets _.client
    res <- H.liftAff $ attempt $ Users.some cli ids

    case res of
      Right users_ -> do
        let addings = Map.fromFoldable $ (view User._id &&& identity) <$> users_
        H.modify_ _{ users = Map.union users addings }
      Left _ -> do
        H.raise $ Failed "Failed to load Users. Retrying..."
        loadUsers
