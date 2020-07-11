module Component.NoticeUI where

import AppPrelude

import Control.Monad.State (class MonadState)
import Data.Lens (Lens, assign, modifying, use)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple as Tuple
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HM


data Notice = Info String | Alert String

noticeBody :: Notice -> String
noticeBody (Info s) = s
noticeBody (Alert s) = s


type ItemId = Int

type Item =
  { notice :: Notice
  , id :: ItemId
  , animated :: Array String
  , pinned :: Boolean
  }

type State =
  { items :: Map ItemId Item
  , lastId :: ItemId
  }

_items :: forall a b r. Lens { items :: a | r } { items :: b | r } a b
_items = prop (SProxy :: SProxy "items")

_lastId :: forall a b r. Lens { lastId :: a | r } { lastId :: b | r } a b
_lastId = prop (SProxy :: SProxy "lastId")

data Action
  = Pin ItemId
  | WaitAndClose ItemId Milliseconds

type Input = Unit

data Query a
  = Post Notice a

data Message =
  Closed Int

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
    , handleQuery = handleQuery
    }
  }
  where
    initialState _ = { items: Map.empty, lastId: 0 }

render ::
  forall m.
  State -> H.ComponentHTML Action () m
render state =
  HH.div
  [ HP.classes [ H.ClassName "fixed-bottom" ] ]
  [ HH.div
    [ HP.class_ $ H.ClassName "container" ]
    $ renderItem <$> Tuple.snd <$> Map.toUnfoldable state.items
  ]

  where
    renderItem item =
      HH.div
      [ HP.classes $ H.ClassName <$> item.animated ]
      [ HH.div
        [ classes item.notice ]
        [ HH.div
          [ HP.class_ $ H.ClassName "position-relative" ]
          [ HH.text $ noticeBody item.notice
          , HH.a
            [ HE.onClick $ const $ Just $ Pin item.id
            , HP.href "#"
            , HP.class_ $ H.ClassName "position-absolute-right position-absolute-top alert-link"
            ]
            [
              renderPinIcon item.pinned
            ]
          ]
        ]
      ]

    classes (Info s) = HP.class_ $ H.ClassName "alert alert-info"
    classes (Alert s) = HP.class_ $ H.ClassName "alert alert-danger"

    renderPinIcon false =
      HH.i [ HP.class_ $ H.ClassName "notice-pin fa fa-map-pin fa-rotate-90" ] []
    renderPinIcon true =
      HH.i [ HP.class_ $ H.ClassName "notice-pin notice-pin-on fa fa-map-pin" ] []

handleAction ::
  forall m.
  MonadAff m =>
  Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Pin id -> do
    item <- findItem id
    item # traverse_ \ item_ ->
      case item_.pinned of
        false -> do
          updateItem id _{ pinned = true, animated = ["notice", "notice-in"] }

        true -> do
          updateItem id _{ pinned = false }
          handleAction $ WaitAndClose id (Milliseconds 1000.0)


  WaitAndClose id ms -> do
    void $ HM.fork do
      H.liftAff $ delay ms
      item <- findItem id
      item # traverse_ \ item_ -> do
        when (not $ item_.pinned) do
          updateItem id _{ animated = ["notice", "notice-out"] }
          H.liftAff $ delay (Milliseconds 500.0)
          deleteItem id


handleQuery ::
  forall m a.
  MonadAff m =>
  Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery = case _ of
  Post notice next -> do
    item <- newItem notice <<< (_ + 1) <$> H.gets _.lastId
    addItem item
    void $ HM.fork do
      H.liftAff $ delay (Milliseconds 100.0)
      updateItem item.id _{ animated = ["notice", "notice-in"] }

    handleAction $ WaitAndClose item.id (Milliseconds 3000.0)
    pure $ Just next


newItem :: Notice -> ItemId -> Item
newItem notice id =
  { notice
  , id
  , animated: ["notice", "notice-out"]
  , pinned: false
  }

addItem :: forall m. MonadState State m => Item -> m Unit
addItem item = do
  assign (_items <<< at item.id) (Just item)
  assign _lastId item.id

findItem :: forall m. MonadState State m => ItemId -> m (Maybe Item)
findItem id =
  use (_items <<< at id)

updateItem :: forall m. MonadState State m => ItemId -> (Item -> Item) -> m Unit
updateItem id updater =
  modifying (_items <<< ix id) updater

deleteItem :: forall m. MonadState State m => ItemId -> m Unit
deleteItem id =
  assign (_items <<< at id) Nothing
