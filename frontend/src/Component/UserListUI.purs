module Component.UserListUI where

import Prelude

import Api as Api
import Api.Users as Users
import Component.Admin.Route as R
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.Util as Util
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.State (class MonadState)
import Data.Array ((!!))
import Data.Array as Array
import Data.DateTime.Locale (Locale)
import Data.Either (Either(Left, Right))
import Data.Int as Int
import Data.Lens (view)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model.User (User(..), UserEntity(..), Users, UserId)
import Model.User as User
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
  | Destroy UserId a

type State =
  { items :: Users
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
    [ HH.text "User List" ]
  , LoadingIndicator.render state.busy
  , HH.table
    [ HP.class_ $ H.ClassName "table table-striped table-bordered table-hover table-sm mb-4" ]
    [
      HH.thead_
      [ renderHeader ]
    , HH.tbody_
      $ renderItem <$> state.items
    ]
  , renderForm
  ]

  where
    renderHeader =
      HH.tr_
      [
        HH.td_
        []
      , HH.td_
        [ HH.text "Name" ]
      , HH.td_
        [ HH.text "Code" ]
      , HH.td_
        [ HH.text "Tel" ]
      , HH.td_
        []
      ]

    renderItem (User { id, code, tel, name, is_admin, created_at }) =
      HH.tr_
      [
        HH.td_
        [ renderIsAdmin is_admin ]
      , HH.td_
        [
          HH.a
          [ HP.href $ R.path $ R.UsersShow id ]
          [ HH.text name ]
        ]
      , HH.td_
        [ HH.text $ show code ]
      , HH.td_
        [ HH.text tel ]
      , HH.dt_
        [
          HH.button
          [ HP.class_ $ H.ClassName "btn btn-outline-danger btn-sm"
          , HE.onClick $ HE.input_ $ Destroy id
          ]
          [
            HH.i [ HP.class_ $ H.ClassName "fa fa-times" ] []
          ]
        ]
      ]

    renderIsAdmin =
      if _
      then
        HH.span
        [ HP.class_ $ H.ClassName "badge badge-danger" ]
        [ HH.text "admin" ]
      else
        HH.span_ []

    renderForm =
      HH.div_
      [
        HH.div
        [ HP.class_ $ H.ClassName "mb-2" ]
        [ HH.textarea
          [ HP.class_ $ H.ClassName "form-control"
          , HP.rows 4
          , HP.value state.populating
          , HP.placeholder "Name, Code, Tel"
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
      , HH.text "Populate"
      ]

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    eval $ Reload next

  SetLocale locale next -> do
    H.modify _{ locale = locale }
    pure next

  Reload next -> do
    busy <- H.gets _.busy
    when (not busy) do
      H.modify _{ busy = true }
      cli <- H.gets _.client
      users <- H.liftAff $ attempt $ Users.index cli

      case users of
        Right users_ ->
          H.modify _{ items = users_ }
        Left _ ->
          H.raise $ Failed "Failed to access api."

      H.modify _{ busy = false }
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
      res <- H.liftAff $ attempt $ Users.destroy cli userId

      case res of
        Right _ -> do
          items <- Array.filter ((userId /= _) <<< view User._id) <$> H.gets _.items
          H.modify _{ items = items }
        Left _ ->
          H.raise $ Failed "Failed to delete user."

      H.modify _{ busy = false }
    pure next

build :: String -> Either String UserEntity
build row = maybe (Left row) Right $ do
  name <- cols !! 0
  code <- Int.fromString =<< cols !! 1
  tel <- cols !! 2
  pure $ UserEntity { name, code, tel, is_admin: false }
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
      user_ <- H.liftAff $ attempt $ Users.create cli user
      case user_ of
        Right user__ -> do
          items <- (_ <> [user__]) <$> H.gets _.items
          H.modify _{ items = items }
        Left _ -> do
          invalids <- (_ <> [row]) <$> H.gets _.invalids
          H.modify _{ invalids = invalids }

    Left _ -> do
      invalids <- (_ <> [row]) <$> H.gets _.invalids
      H.modify _{ invalids = invalids }
