module Component.Admin.PestListPage where

import AppPrelude

import Api as Api
import Api.Client (Client)
import Api.Crops as Crops
import Api.Pests as Pests
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.PopulateUI as PopulateUI
import Component.Util as Util
import Data.Array as Array
import Data.Lens (Lens, _Just, modifying, view)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.String (Pattern(..))
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n.Ja as Ja
import Model.Crop (Crop(..), CropId)
import Model.Crop as Crop
import Model.DateTime (Locale)
import Model.Pest (Pest(..), PestEntity(..), PestId)
import Model.Pest as Pest


data Action
  = Initialize
  | SetLocale Locale
  | Reload
  | SelectCrop Crop
  | Destroy PestId
  | HandlePopulate (PopulateUI.Message Pest)

type State =
  { items :: Array Pest
  , crops :: Array Crop
  , selectedCrop :: Maybe Crop
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
  ( populate :: H.Slot (Const Void) (PopulateUI.Message Pest) (Maybe CropId)
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
      , crops: []
      , selectedCrop: Nothing
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
  [ HH.h1_
    [ HH.text Ja.pest_list ]
  , LoadingIndicator.render state.busy
  , HH.div
    [ HP.class_ $ H.ClassName "mb-2" ]
    [ HH.div_
      $ renderCrop <$> state.crops
    ]
  , HH.div
    [ HP.class_ $ H.ClassName "mb-2" ]
    [ HH.div_
      $ renderItem <$> state.items
    ]
  , HH.slot (SProxy :: _ "populate") (state.selectedCrop ^? _Just <<< Crop._id) PopulateUI.ui populateInput $ Just <<< HandlePopulate
  ]

  where
    populateInput =
      { creater: creater state.client state.selectedCrop
      , placeholder: Ja.pest_label
      }

    renderCrop crop@(Crop { id, label }) =
      HH.button
      [ HP.class_ $ H.ClassName "btn mr-2"
        <> (wrap $ bool " btn-outline-info" " btn-info" (Just id == state.selectedCrop ^? _Just <<< Crop._id))
      , HE.onClick $ const $ Just $ SelectCrop crop
      ]
      [ HH.text label
      ]

    renderItem (Pest { id, label }) =
      HH.div
      [ HP.class_ $ H.ClassName "btn-group mr-2" ]
      [ HH.span
        [ HP.class_ $ H.ClassName "input-group-addon" ]
        [ HH.text label
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
        Right crops -> do
          H.modify_ _{ crops = crops }
          H.liftAff (attempt $ Pests.index cli) >>= case _ of
            Right items -> H.modify_ _{ items = items }
            Left _ -> H.raise $ Failed "Failed to access api."
        Left _ -> H.raise $ Failed "Failed to access api."

  SelectCrop crop@(Crop { id }) -> do
    selectedCrop <- H.gets _.selectedCrop
    let crop' = bool (Just crop) Nothing (Just id == selectedCrop ^? _Just <<< Crop._id)
    H.modify_ _{ selectedCrop = crop' }
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      let filters = Object.empty # at "crop" .~ (Api.Eq_ <$> crop' ^? _Just <<< Crop._label)
      H.liftAff (attempt $ Pests.filter cli filters) >>= case _ of
        Right items -> H.modify_ _{ items = items }
        Left _ -> H.raise $ Failed "Failed to access api."

  Destroy pestId -> do
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      res <- H.liftAff $ attempt $ Pests.destroy cli pestId

      case res of
        Right _ -> do
          items <- Array.filter ((pestId /= _) <<< view Pest._id) <$> H.gets _.items
          H.modify_ _{ items = items }
        Left _ ->
          H.raise $ Failed "Failed to delete pest."

  HandlePopulate (PopulateUI.Created pest) -> do
    modifying _items $ flip Array.snoc pest

  HandlePopulate (PopulateUI.Failed _) -> do
    H.raise $ Failed "Failed to create some pests."


creater :: forall m. MonadAff m => Client -> Maybe Crop -> String -> m (Maybe Pest)
creater cli crop row = do
  let entity = do
        label <- Array.head $ String.trim <$> String.split (Pattern ",") row
        pure $ PestEntity { label, crop: (_.label <<< unwrap) <$> crop }
  entity # maybe (pure Nothing) \ entity' -> do
    map hush $ H.liftAff $ attempt $ Pests.create cli entity'
