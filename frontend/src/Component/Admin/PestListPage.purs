module Component.Admin.PestListPage where

import Prelude

import Api as Api
import Api.Pests as Pests
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.PopulateUI as PopulateUI
import Component.Util as Util
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (class MonadAff)
import Data.Array ((!!))
import Data.Array as Array
import Data.Const (Const)
import Data.DateTime.Locale (Locale)
import Data.Either (Either(Left, Right), hush)
import Data.Lens (Lens, modifying, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Data.Prism (type (<\/>), type (\/))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n.Ja as Ja
import Model.Pest (Pest(..), PestEntity(..), PestId)
import Model.Pest as Pest
import Network.HTTP.Affjax (AJAX)


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


data Query a
  = Initialize a
  | SetLocale Locale a
  | Reload a
  | Destroy PestId a
  | HandlePopulate (PopulateUI.Message Pest) a

type State =
  { items :: Array Pest
  , client :: Api.Client
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
  { client :: Api.Client
  , locale :: Locale
  }

data Message
  = Failed String

type ChildQuery
  = PopulateUI.Query
    <\/> Const Void

type ChildSlot
  = PopulateUI.Slot
    \/ Void

cpPopulate :: CP.ChildPath PopulateUI.Query ChildQuery PopulateUI.Slot ChildSlot
cpPopulate = CP.cp1

type Eff_ eff = Aff (ajax :: AJAX | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleParentComponent
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
    , populating: ""
    , invalids: []
    }

render :: forall eff. State -> H.ParentHTML Query ChildQuery ChildSlot (Eff_ eff)
render state =
  HH.div_
  [
    HH.h1_
    [ HH.text Ja.pest_list ]
  , LoadingIndicator.render state.busy
  , HH.div
    [ HP.class_ $ H.ClassName "mb-2" ]
    [
      HH.div_
      $ renderItem <$> state.items
    ]
  , HH.slot' cpPopulate PopulateUI.Slot PopulateUI.ui populateInput $ HE.input HandlePopulate
  ]

  where
    populateInput =
      { creater: creater state.client
      , placeholder: Ja.pest_label
      }

    renderItem (Pest { id, label }) =
      HH.div
      [ HP.class_ $ H.ClassName "btn-group mr-2" ]
      [
        HH.span
        [ HP.class_ $ H.ClassName "input-group-addon" ]
        [
          HH.text label
        ]
      , HH.button
        [ HP.class_ $ H.ClassName "btn btn-secondary btn-outline"
        , HE.onClick $ HE.input_ $ Destroy id
        ]
        [
          HH.i [ HP.class_ $ H.ClassName "fa fa-times" ] []
        ]
      ]

eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    eval $ Reload next

  SetLocale locale next -> do
    H.modify _{ locale = locale }
    pure next

  Reload next -> do
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      res <- H.liftAff $ attempt $ Pests.index cli

      case res of
        Right items ->
          H.modify _{ items = items }
        Left _ ->
          H.raise $ Failed "Failed to access api."

    pure next

  Destroy pestId next -> do
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      res <- H.liftAff $ attempt $ Pests.destroy cli pestId

      case res of
        Right _ -> do
          items <- Array.filter ((pestId /= _) <<< view Pest._id) <$> H.gets _.items
          H.modify _{ items = items }
        Left _ ->
          H.raise $ Failed "Failed to delete pest."

    pure next

  HandlePopulate (PopulateUI.Created pest) next -> do
    modifying _items $ flip Array.snoc pest
    pure next

  HandlePopulate (PopulateUI.Failed _) next -> do
    H.raise $ Failed "Failed to create some pests."
    pure next


creater :: forall eff m. MonadAff (ajax :: AJAX | eff) m => Api.Client -> String -> m (Maybe Pest)
creater cli row =
  maybe (pure Nothing) (map hush <<< H.liftAff <<< attempt <<< Pests.create cli) entity

  where
    cols = String.trim <$> String.split (Pattern ",") row
    entity = do
      label <- cols !! 0
      pure $ PestEntity { label }
