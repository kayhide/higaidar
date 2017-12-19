module Component.PagerUI where

import Prelude

import Data.Array as Array
import Data.Lens (Lens)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


type Pager
  = { current :: Int
    , per :: Int
    , count :: Int
    }

_current :: forall a b r. Lens { current :: a | r } { current :: b | r } a b
_current = prop (SProxy :: SProxy "current")

_per :: forall a b r. Lens { per :: a | r } { per :: b | r } a b
_per = prop (SProxy :: SProxy "per")

_count :: forall a b r. Lens { count :: a | r } { count :: b | r } a b
_count = prop (SProxy :: SProxy "count")


type Config
  = { window :: Int
    , labels :: Labels
    }

type Labels
  = { first :: String
    , previous :: String
    , next :: String
    , last :: String
    , truncate :: String
    }

defaultConfig :: Config
defaultConfig =
  { window: 4
  , labels: { first: "|<"
            , previous: "<"
            , next: ">"
            , last: ">|"
            , truncate: "..."
            }
  }

type State =
  { config :: Config
  , pager :: Pager
  }

data Query a
  = Select Int a
  | SetPager Pager a

type Input = Pager

data Message
  = Selected Int


ui :: forall eff. H.Component HH.HTML Query Input Message eff
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
    where
      initialState pager =
        { config: defaultConfig
        , pager
        }

render :: State -> H.ComponentHTML Query
render state = case range of
  [_] -> HH.div_ []
  _ ->
    HH.nav_
    [
      HH.ul
      [ HP.class_ $ H.ClassName "pagination" ]
      $  [ renderLinkCell (labels.first) first
         , renderLinkCell (labels.previous) $ max first (current - 1)
         ]
      <> (if first < lo then [ renderDisabledCell labels.truncate ] else [])
      <> (renderNumCell <$> los)
      <> [ renderActiveCell $ show current ]
      <> (renderNumCell <$> his)
      <> (if hi < last then [ renderDisabledCell labels.truncate ] else [])
      <> [ renderLinkCell (labels.next) $ min last (current + 1)
         , renderLinkCell (labels.last) $ last
         ]
    ]

  where
    config = state.config
    labels = config.labels
    pager = state.pager
    current = pager.current
    first = 1
    last = 1 + (div pager.count pager.per)
    lo = max first $ current - config.window
    hi = min last $ current + config.window
    range = Array.range lo hi
    los = Array.takeWhile (_ < current) range
    his = Array.dropWhile (_ <= current) range

    renderLinkCell label page =
      HH.li
      [ HP.class_ $ H.ClassName $ "page-item" <> if page == current then " disabled" else "" ]
      [
        HH.button
        [ HP.class_ $ H.ClassName "page-link"
        , HE.onClick $ HE.input_ $ Select page
        ]
        [ HH.text label ]
      ]

    renderNumCell i = renderLinkCell (show i) i

    renderActiveCell label =
      HH.li
      [ HP.class_ $ H.ClassName "page-item active" ]
      [
        HH.button
        [ HP.class_ $ H.ClassName "page-link"
        ]
        [ HH.text label ]
      ]

    renderDisabledCell label =
      HH.li
      [ HP.class_ $ H.ClassName "page-item disabled" ]
      [
        HH.button
        [ HP.class_ $ H.ClassName "page-link"
        ]
        [ HH.text label ]
      ]


eval :: forall eff. Query ~> H.ComponentDSL State Query Message eff
eval = case _ of
  Select i next -> do
    H.raise $ Selected i
    pure next

  SetPager pager next -> do
    H.modify _{ pager = pager }
    pure next
