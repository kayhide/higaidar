module Component.PagerUI where

import AppPrelude

import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


type Pager
  = { current :: Int
    , per :: Int
    , count :: Int
    }


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

data Action
  = Select Int


type Input = Pager


data Query a
  = SetPager Pager a

data Message
  = Selected Int


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
    , handleQuery = handleQuery
    }
  }
  where
    initialState pager =
      { config: defaultConfig
      , pager
      }

render ::
  forall m.
  MonadEffect m =>
  State -> H.ComponentHTML Action () m
render state = case range of
  [_] -> HH.div_ []
  _ ->
    HH.nav_
    [ HH.ul
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
      [ HH.button
        [ HP.class_ $ H.ClassName "page-link"
        , HE.onClick $ const $ Just $ Select page
        ]
        [ HH.text label ]
      ]

    renderNumCell i = renderLinkCell (show i) i

    renderActiveCell label =
      HH.li
      [ HP.class_ $ H.ClassName "page-item active" ]
      [ HH.button
        [ HP.class_ $ H.ClassName "page-link"
        ]
        [ HH.text label ]
      ]

    renderDisabledCell label =
      HH.li
      [ HP.class_ $ H.ClassName "page-item disabled" ]
      [ HH.button
        [ HP.class_ $ H.ClassName "page-link"
        ]
        [ HH.text label ]
      ]


handleAction ::
  forall m.
  MonadAff m =>
  Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Select i -> do
    H.raise $ Selected i


handleQuery ::
  forall m a.
  Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery = case _ of
  SetPager pager next -> do
    H.modify_ _{ pager = pager }
    pure $ Just next
