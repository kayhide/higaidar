module Component.Admin.UserListPage where

import Prelude

import Api as Api
import Api.Users as Users
import Component.Admin.Route as R
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.PagerUI (_count, _current)
import Component.PagerUI as PagerUI
import Component.PopulateUI as PopulateUI
import Component.Util as Util
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (class MonadAff)
import Data.Array ((!!))
import Data.Array as Array
import Data.Const (Const)
import Data.DateTime.Locale (Locale)
import Data.Either (Either(Left, Right), hush)
import Data.Lens (Lens, assign, modifying, view)
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
  | Destroy UserId a
  | HandlePager PagerUI.Message a
  | HandlePopulate (PopulateUI.Message User) a

type State =
  { items :: Array User
  , client :: Api.Client
  , locale :: Locale
  , busy :: Boolean
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

type ChildQuery
  = PagerUI.Query
    <\/> PopulateUI.Query
    <\/> Const Void

type ChildSlot
  = PagerUI.Slot
    \/ PopulateUI.Slot
    \/ Void

cpPager :: CP.ChildPath PagerUI.Query ChildQuery PagerUI.Slot ChildSlot
cpPager = CP.cp1

cpPopulate :: CP.ChildPath PopulateUI.Query ChildQuery PopulateUI.Slot ChildSlot
cpPopulate = CP.cp2

type Eff_ eff = Aff (ajax :: AJAX | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleParentComponent
    { initialState
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
  , HH.slot' cpPager PagerUI.Slot PagerUI.ui state.pager $ HE.input HandlePager
  , HH.slot' cpPopulate PopulateUI.Slot PopulateUI.ui populateInput $ HE.input HandlePopulate
  ]

  where
    populateInput =
      { creater: creater state.client
      , placeholder: Array.intercalate ", " [ Ja.user_name, Ja.user_code, Ja.user_tel ]
      }

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
          void <<< H.query' cpPager PagerUI.Slot <<< H.action <<< PagerUI.SetPager =<< H.gets _.pager
        Left _ ->
          H.raise $ Failed "Failed to access api."
      pure unit

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

  HandlePopulate (PopulateUI.Created user) next -> do
    modifying _items $ flip Array.snoc user
    pure next

  HandlePopulate (PopulateUI.Failed _) next -> do
    H.raise $ Failed "Failed to create some users."
    pure next


creater :: forall eff m. MonadAff (ajax :: AJAX | eff) m => Api.Client -> String -> m (Maybe User)
creater cli row =
  maybe (pure Nothing) (map hush <<< H.liftAff <<< attempt <<< Users.create cli) entity

  where
    cols = String.trim <$> String.split (Pattern ",") row
    entity = do
      name <- cols !! 0
      code <- cols !! 1
      tel <- cols !! 2
      pure $ UserEntity { name, code, tel, is_admin: false }