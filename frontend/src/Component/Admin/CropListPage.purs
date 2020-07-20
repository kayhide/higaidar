module Component.Admin.CropListPage where

import AppPrelude

import Api.Client (Client)
import Api.Crops as Crops
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.PopulateUI as PopulateUI
import Component.Util as Util
import Data.Array as Array
import Data.Lens (Lens, modifying, view)
import Data.Lens.Record (prop)
import Data.String (Pattern(..))
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n.Ja as Ja
import Model.Crop (Crop(..), CropEntity(..), CropId)
import Model.Crop as Crop
import Model.DateTime (Locale)


data Action
  = Initialize
  | SetLocale Locale
  | Reload
  | Destroy CropId
  | HandlePopulate (PopulateUI.Message Crop)

type State =
  { items :: Array Crop
  , client :: Client
  , locale :: Locale
  , busy :: Boolean
  , populating :: String
  , invalids :: Array String
  }

_items :: forall a b r. Lens { items :: a | r } { items :: b | r } a b
_items = prop (SProxy :: SProxy "items")

_invalids :: forall a b r. Lens { invalids :: a | r } { invalids :: b | r } a b
_invalids = prop (SProxy :: SProxy "invalids")

type Input =
  { client :: Client
  , locale :: Locale
  }

type Query = Const Void

data Message
  = Failed String

type ChildSlots =
  ( populate :: H.Slot (Const Void) (PopulateUI.Message Crop) Unit
  )

ui ::
  forall m.
  MonadAff m =>
  H.Component HH.HTML Query Input Message m
ui =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Reload
    }
  }

  where
    initialState { client, locale } =
      { items: []
      , client
      , locale
      , busy: false
      , populating: ""
      , invalids: []
      }

render ::
  forall m.
  MonadAff m =>
  State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
  [
    HH.h1_
    [ HH.text Ja.crop_list ]
  , LoadingIndicator.render state.busy
  , HH.div
    [ HP.class_ $ H.ClassName "mb-2" ]
    [ HH.div_
      $ renderItem <$> state.items
    ]
  , HH.slot (SProxy :: _ "populate") unit PopulateUI.ui populateInput $ Just <<< HandlePopulate
  ]

  where
    populateInput =
      { creater: creater state.client
      , placeholder: Ja.crop_label
      }

    renderItem (Crop { id, label }) =
      HH.div
      [ HP.class_ $ H.ClassName "btn-group mr-2" ]
      [ HH.span
        [ HP.class_ $ H.ClassName "input-group-addon" ]
        [
          HH.text label
        ]
      , HH.button
        [ HP.class_ $ H.ClassName "btn btn-secondary btn-outline"
        , HE.onClick $ const $ Just $ Destroy id
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fa fa-times" ] []
        ]
      ]


handleAction ::
  forall m.
  MonadAff m =>
  Action -> H.HalogenM State Action ChildSlots Message m Unit
handleAction = case _ of
  Initialize -> do
    handleAction Reload

  SetLocale locale -> do
    H.modify_ _{ locale = locale }

  Reload -> do
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      H.liftAff (attempt $ Crops.index cli) >>= case _ of
        Right items -> H.modify_ _{ items = items }
        Left _ -> H.raise $ Failed "Failed to access api."

  Destroy cropId -> do
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      res <- H.liftAff $ attempt $ Crops.destroy cli cropId

      case res of
        Right _ -> do
          items <- Array.filter ((cropId /= _) <<< view Crop._id) <$> H.gets _.items
          H.modify_ _{ items = items }
        Left _ ->
          H.raise $ Failed "Failed to delete crop"

  HandlePopulate (PopulateUI.Created crop) -> do
    modifying _items $ flip Array.snoc crop

  HandlePopulate (PopulateUI.Failed _) -> do
    H.raise $ Failed "Failed to create some crops."


creater :: forall m. MonadAff m => Client -> String -> m (Maybe Crop)
creater cli row = do
  let entity = do
        label <- Array.head $ String.trim <$> String.split (Pattern ",") row
        pure $ CropEntity { label }
  entity # maybe (pure Nothing) \ entity' ->
    map hush $ H.liftAff $ attempt $ Crops.create cli entity'
