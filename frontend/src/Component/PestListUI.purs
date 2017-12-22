module Component.PestListUI where

import Prelude

import Api as Api
import Api.Pests as Pests
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.Util as Util
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State (class MonadState)
import Data.Array ((!!))
import Data.Array as Array
import Data.DateTime.Locale (Locale)
import Data.Either (Either(Left, Right), either)
import Data.Lens (view)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse_)
import Halogen as H
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
  | SetPopulating String a
  | Populate a
  | Destroy PestId a

type State =
  { items :: Array Pest
  , client :: Api.Client
  , locale :: Locale
  , busy :: Boolean
  , populating :: String
  , invalids :: Array String
  }

type Input =
  { client :: Api.Client
  , locale :: Locale
  }

data Message
  = Failed String


type Eff_ eff = Aff (ajax :: AJAX | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleComponent
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

render :: State -> H.ComponentHTML Query
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
  , renderForm
  ]

  where
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

    renderForm =
      HH.div_
      [
        HH.div
        [ HP.class_ $ H.ClassName "mb-1" ]
        [ HH.textarea
          [ HP.class_ $ H.ClassName "form-control"
          , HP.rows 4
          , HP.value state.populating
          , HP.placeholder Ja.pest_label
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

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    eval $ Reload next

  SetLocale locale next -> do
    H.modify _{ locale = locale }
    pure next

  Reload next -> do
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      users <- H.liftAff $ attempt $ Pests.index cli

      case users of
        Right users_ ->
          H.modify _{ items = users_ }
        Left _ ->
          H.raise $ Failed "Failed to access api."

    pure next

  SetPopulating s next -> do
    H.modify _{ populating = s}
    pure next

  Populate next -> do
    Util.whenNotBusy_ do
      H.modify _{ invalids = [] }
      text <- H.gets _.populating
      let rows = Array.filter (not String.null) $ String.trim <$> String.split (Pattern "\n") text

      cli <- H.gets _.client
      traverse_ (tryCreate cli) rows

      invalids <- H.gets _.invalids
      H.modify _{ populating = String.joinWith "\r\n" invalids }
      when (not $ Array.null invalids) do
        H.raise $ Failed "Failed to create some users."

    pure next

  Destroy userId next -> do
    busy <- H.gets _.busy
    when (not busy) do
      H.modify _{ busy = true }
      cli <- H.gets _.client
      res <- H.liftAff $ attempt $ Pests.destroy cli userId

      case res of
        Right _ -> do
          items <- Array.filter ((userId /= _) <<< view Pest._id) <$> H.gets _.items
          H.modify _{ items = items }
        Left _ ->
          H.raise $ Failed "Failed to delete user."

      H.modify _{ busy = false }
    pure next

onLeft :: forall e m. Monad m => String -> Either e ~> ExceptT String m
onLeft s = either (throwError <<< const s) pure

build :: String -> Either String PestEntity
build row = maybe (Left row) Right $ do
  label <- cols !! 0
  pure $ PestEntity { label }
  where
    cols = String.trim <$> String.split (Pattern ",") row


tryCreate :: forall eff m.
             Monad m =>
             MonadAff (ajax :: AJAX | eff) m =>
             MonadState State m =>
             Api.Client -> String -> m Unit
tryCreate cli row = do
  case build row of
    Right user -> do
      user_ <- H.liftAff $ attempt $ Pests.create cli user
      case user_ of
        Right user__ -> do
          items <- (_ <> [user__]) <$> H.gets _.items
          H.modify _{ items = items }
        Left s -> do
          invalids <- (_ <> [row]) <$> H.gets _.invalids
          H.modify _{ invalids = invalids }

    Left _ -> do
      invalids <- (_ <> [row]) <$> H.gets _.invalids
      H.modify _{ invalids = invalids }
