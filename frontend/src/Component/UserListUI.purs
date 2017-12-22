module Component.UserListUI where

import Prelude

import Api as Api
import Api.Users as Users
import Component.Admin.Route as R
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.PagerUI (_count, _current)
import Component.PagerUI as PagerUI
import Component.Util as Util
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.State (class MonadState)
import Data.Array ((!!))
import Data.Array as Array
import Data.DateTime.Locale (Locale)
import Data.Either (Either(Left, Right))
import Data.Lens (Lens, assign, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n.Ja as Ja
import Model.User (User(User), UserEntity(UserEntity), UserId)
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
  | HandlePager PagerUI.Message a

type State =
  { items :: Array User
  , client :: Api.Client
  , locale :: Locale
  , busy :: Boolean
  , populating :: String
  , invalids :: Array String
  , pager :: PagerUI.Pager
  }

_items :: forall a b r. Lens { items :: a | r } { items :: b | r } a b
_items = prop (SProxy :: SProxy "items")

_pager :: forall a b r. Lens { pager :: a | r } { pager :: b | r } a b
_pager = prop (SProxy :: SProxy "pager")

type Input =
  { client :: Api.Client
  , locale :: Locale
  }

data Message
  = Failed String

type ChildQuery = PagerUI.Query
type ChildSlot = PagerUI.Slot

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
  , pager
  }
  where
    pager =
      { current: 1
      , per: 50
      , count: 0
      }


render :: forall eff. State -> H.ParentHTML Query ChildQuery ChildSlot (Eff_ eff)
render state =
  HH.div_
  [
    HH.h1_
    [ HH.text Ja.user_list ]
  , LoadingIndicator.render state.busy
  , HH.table
    [ HP.class_ $ H.ClassName "table table-striped table-bordered table-hover table-sm mb-4" ]
    [
      HH.thead_
      [ renderHeader ]
    , HH.tbody_
      $ renderItem <$> state.items
    ]
  , HH.slot PagerUI.Slot PagerUI.ui state.pager $ HE.input HandlePager
  , renderForm
  ]

  where
    renderHeader =
      HH.tr_
      [
        HH.td_
        []
      , HH.td_
        [ HH.text Ja.user_name ]
      , HH.td_
        [ HH.text Ja.user_code ]
      , HH.td_
        [ HH.text Ja.user_tel ]
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
        [ HH.text code ]
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
        [ HP.class_ $ H.ClassName "mb-1" ]
        [ HH.textarea
          [ HP.class_ $ H.ClassName "form-control"
          , HP.rows 4
          , HP.value state.populating
          , HP.placeholder $ Array.intercalate ", " [ Ja.user_name, Ja.user_code, Ja.user_tel ]
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
      pager@{ current, per } <- H.gets _.pager
      let offset = (current - 1) * per
      res <- H.liftAff $ attempt $ Users.page cli offset per

      case res of
        Right { body, range: { count } } -> do
          assign _items body
          assign (_pager <<< _count) count
          void <<< H.query PagerUI.Slot <<< H.action <<< PagerUI.SetPager =<< H.gets _.pager
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
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      res <- H.liftAff $ attempt $ Users.destroy cli userId

      case res of
        Right _ -> do
          items <- Array.filter ((userId /= _) <<< view User._id) <$> H.gets _.items
          H.modify _{ items = items }
        Left _ ->
          H.raise $ Failed "Failed to delete user."

    pure next

  HandlePager (PagerUI.Selected page) next -> do
    assign (_pager <<< _current) page
    eval $ Reload next

build :: String -> Either String UserEntity
build row = maybe (Left row) Right $ do
  name <- cols !! 0
  code <- cols !! 1
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
