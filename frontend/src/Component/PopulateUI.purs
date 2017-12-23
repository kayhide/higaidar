module Component.PopulateUI where

import Prelude

import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.Util as Util
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (runExceptT)
import Data.Array as Array
import Data.Either (Either(Left, Right))
import Data.Lens (Lens, modifying)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (Pattern(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n.Ja as Ja


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type Creater eff a = String -> Aff eff (Maybe a)

type State eff a =
  { creater :: Creater eff a
  , placeholder :: String
  , busy :: Boolean
  , populating :: String
  , items :: Array a
  , invalids :: Array String
  }

_items :: forall a b r. Lens { items :: a | r } { items :: b | r } a b
_items = prop (SProxy :: SProxy "items")

_invalids :: forall a b r. Lens { invalids :: a | r } { invalids :: b | r } a b
_invalids = prop (SProxy :: SProxy "invalids")

data Query a
  = Initialize a
  | SetPopulating String a
  | Populate a

type Input eff a =
  { creater :: Creater eff a
  , placeholder :: String
  }

data Message a
  = Created a
  | Failed (Array String)


ui :: forall eff a. H.Component HH.HTML Query (Input eff a) (Message a) (Aff eff)
ui =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where
    initialState { creater, placeholder } =
      { creater
      , placeholder
      , busy: false
      , populating: ""
      , items: []
      , invalids: []
      }

render :: forall eff a. State eff a -> H.ComponentHTML Query
render state =
  HH.div_
  [
    LoadingIndicator.render state.busy
  , renderForm
  ]

  where
    renderForm =
      HH.div_
      [
        HH.div
        [ HP.class_ $ H.ClassName "mb-1" ]
        [ HH.textarea
          [ HP.class_ $ H.ClassName "form-control"
          , HP.rows 4
          , HP.value state.populating
          , HP.placeholder state.placeholder
          , HE.onValueInput $ HE.input SetPopulating
          ]
        ]
      , renderSubmitButton
      ]

    renderSubmitButton =
      HH.button
      [ HP.class_ $ H.ClassName "btn btn-success"
      , HE.onClick $ HE.input_ Populate
      ]
      [
        HH.i [ HP.class_ $ H.ClassName "fa fa-plus mr-2" ] []
      , HH.text Ja.populate
      ]

eval :: forall eff a. Query ~> H.ComponentDSL (State eff a) Query (Message a) (Aff eff)
eval = case _ of
  Initialize next -> do
    pure next

  SetPopulating s next -> do
    H.modify _{ populating = s }
    pure next

  Populate next -> do
    Util.whenNotBusy_ do
      H.modify _{ items = [], invalids = [] }
      text <- H.gets _.populating
      let rows = Array.filter (not String.null) $ String.trim <$> String.split (Pattern "\n") text

      traverse_ tryCreate rows

      invalids <- H.gets _.invalids
      H.modify _{ populating = String.joinWith "\r\n" invalids }
      when (not $ Array.null invalids) do
        H.raise $ Failed invalids

    pure next

  where
    tryCreate row = do
      creater <- H.gets _.creater
      res <- runExceptT do
        Util.exceptNothing =<<
        Util.exceptLeft =<< (H.liftAff $ attempt $ creater row)

      case res of
        Right x ->
          H.raise $ Created x
        Left _ ->
          modifying _invalids $ flip Array.snoc row
