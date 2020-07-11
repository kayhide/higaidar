module Component.PopulateUI where

import AppPrelude

import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.Util as Util
import Control.Monad.Except (runExceptT)
import Data.Array as Array
import Data.String (Pattern(..))
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n.Ja as Ja


type Creater a = String -> Aff (Maybe a)

type State a =
  { creater :: Creater a
  , placeholder :: String
  , busy :: Boolean
  , populating :: String
  , items :: Array a
  , invalids :: Array String
  }

data Action
  = SetPopulating String
  | Populate

type Input a =
  { creater :: Creater a
  , placeholder :: String
  }

data Message a
  = Created a
  | Failed (Array String)


ui ::
  forall a m.
  MonadAff m =>
  H.Component HH.HTML (Const Void) (Input a) (Message a) m
ui =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    }
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

render ::
  forall m a.
  MonadEffect m =>
  State a -> H.ComponentHTML Action () m
render state =
  HH.div_
  [ LoadingIndicator.render state.busy
  , renderForm
  ]

  where
    renderForm =
      HH.div_
      [ HH.div
        [ HP.class_ $ H.ClassName "mb-1" ]
        [ HH.textarea
          [ HP.class_ $ H.ClassName "form-control"
          , HP.rows 4
          , HP.value state.populating
          , HP.placeholder state.placeholder
          , HE.onValueInput $ Just <<< SetPopulating
          ]
        ]
      , renderSubmitButton
      ]

    renderSubmitButton =
      HH.button
      [ HP.class_ $ H.ClassName "btn btn-success"
      , HE.onClick $ const (Just Populate)
      ]
      [ HH.i [ HP.class_ $ H.ClassName "fa fa-plus mr-2" ] []
      , HH.text Ja.populate
      ]

handleAction ::
  forall a m.
  MonadAff m =>
  Action -> H.HalogenM (State a) Action () (Message a) m Unit
handleAction = case _ of
  SetPopulating s -> do
    H.modify_ _{ populating = s }

  Populate -> do
    Util.whenNotBusy_ do
      H.modify_ _{ items = [], invalids = [] }
      text <- H.gets _.populating
      let rows = Array.filter (not String.null) $ String.trim <$> String.split (Pattern "\n") text

      traverse_ tryCreate rows

      invalids <- H.gets _.invalids
      H.modify_ _{ populating = String.joinWith "\r\n" invalids }
      when (not $ Array.null invalids) do
        H.raise $ Failed invalids

  where
    tryCreate row = do
      creater <- H.gets _.creater
      res <- runExceptT do
        Util.exceptNothing =<< Util.exceptLeft =<< (H.liftAff $ attempt $ creater row)

      case res of
        Right x ->
          H.raise $ Created x
        Left _ -> do
          invalids <- H.gets _.invalids
          H.modify_ _{ invalids = Array.snoc invalids row }
