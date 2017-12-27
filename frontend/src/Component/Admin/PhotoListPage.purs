module Component.Admin.PhotoListPage where

import Prelude

import Api as Api
import Api.Photos as Photos
import Api.Users as Users
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.UploadUI as UploadUI
import Component.Util as Util
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.State (execState)
import Data.Array as Array
import Data.DateTime.Locale (Locale)
import Data.Either (Either(Left, Right))
import Data.Lens (assign, view, (^?))
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, maybe)
import Data.Profunctor.Strong ((&&&))
import Data.StrMap as StrMap
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HM
import I18n.Ja as Ja
import Model.Photo (Photo(Photo), PhotoId)
import Model.Photo as Photo
import Model.User (User(..), UserId)
import Model.User as User
import Network.HTTP.Affjax (AJAX)


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


data Query a
  = Initialize a
  | Reload a
  | Destroy PhotoId a
  | ToggleDeleting a
  | SelectUser UserId a
  | UnselectUser UserId a
  | SelectPest String a
  | UnselectPest String a

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
      , users: Map.empty
      , client
      , locale
      , deleting: false
      , busy: false
      , selectedUsers: []
      , selectedPests: []
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
        , HE.onClick $ HE.input_ $ UnselectUser userId
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
        , HE.onClick $ HE.input_ $ UnselectPest pest
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
        , HE.onClick $ HE.input_ $ ToggleDeleting
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
        , HE.onClick $ HE.input_ $ SelectUser id
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
        , HE.onClick $ HE.input_ $ SelectPest pest
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
      { client, selectedUsers, selectedPests } <- H.get
      let filters = flip execState StrMap.empty $ do
            when (not $ Array.null selectedUsers) $
              assign (at "user_id") $ Just $ Api.In_ $ show <$> selectedUsers
            when (not $ Array.null selectedPests) $
              assign (at "pest") $ Just $ Api.In_ selectedPests

      photos <- case StrMap.isEmpty filters of
        true ->
          H.liftAff $ attempt $ Photos.index client
        false ->
          H.liftAff $ attempt $ Photos.filter client filters

      case photos of
        Right photos_ -> do
          H.modify _{ items = photos_ }
          void $ HM.fork loadUsers
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

  SelectUser userId next -> do
    { selectedUsers } <- H.get
    if selectedUsers == [userId]
      then
        pure next
      else do
        H.modify _{ selectedUsers = Array.nub $ Array.cons userId selectedUsers }
        eval $ Reload next

  UnselectUser userId next -> do
    { selectedUsers } <- H.get
    H.modify _{ selectedUsers = Array.delete userId selectedUsers }
    eval $ Reload next

  SelectPest pest next -> do
    { selectedPests } <- H.get
    if selectedPests == [pest]
      then
        pure next
      else do
        H.modify _{ selectedPests = Array.nub $ Array.cons pest selectedPests }
        eval $ Reload next

  UnselectPest pest next -> do
    { selectedPests } <- H.get
    H.modify _{ selectedPests = Array.delete pest selectedPests }
    eval $ Reload next

loadUsers :: forall eff. H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff) Unit
loadUsers = do
  { items, users } <- H.get
  let ids = Array.difference (Array.nub $ view Photo._user_id <$> items) (Array.fromFoldable $ Map.keys users)

  when (not Array.null ids) $ do
    cli <- H.gets _.client
    res <- H.liftAff $ attempt $ Users.some cli ids

    case res of
      Right users_ -> do
        let addings = Map.fromFoldable $ (view User._id &&& id) <$> users_
        H.modify _{ users = Map.union users addings }
      Left _ -> do
        H.raise $ Failed "Failed to load Users. Retrying..."
        loadUsers
