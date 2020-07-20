module Component.Admin.UserListPage where

import AppPrelude

import Api.Client (Client)
import Api.Users as Users
import Component.Admin.Route as R
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.PagerUI as PagerUI
import Component.PopulateUI as PopulateUI
import Component.Util as Util
import Data.Array ((!!))
import Data.Array as Array
import Data.Lens (view)
import Data.String (Pattern(..))
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n.Ja as Ja
import Model.DateTime (Locale)
import Model.User (User(..), UserEntity(..), UserId)
import Model.User as User


data Action
  = SetLocale Locale
  | Reload
  | Destroy UserId
  | HandlePager PagerUI.Message
  | HandlePopulate (PopulateUI.Message User)

type State =
  { items :: Array User
  , client :: Client
  , locale :: Locale
  , busy :: Boolean
  , pager :: PagerUI.Pager
  }

type Input =
  { client :: Client
  , locale :: Locale
  }

type Query = Const Void

data Message
  = Failed String

type ChildSlots =
  ( pager :: H.Slot PagerUI.Query PagerUI.Message Unit
  , populate :: H.Slot (Const Void) (PopulateUI.Message User) Unit
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
      , client
      , locale
      , busy: false
      , pager:
        { current: 1
        , per: 50
        , count: 0
        }
      }


render ::
  forall m.
  MonadAff m =>
  State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
  [ HH.h1_
    [ HH.text Ja.user_list ]
  , LoadingIndicator.render state.busy
  , HH.table
    [ HP.class_ $ H.ClassName "table table-striped table-bordered table-hover table-sm mb-4" ]
    [ HH.thead_
      [ renderHeader ]
    , HH.tbody_
      $ renderItem <$> state.items
    ]
  , HH.slot (SProxy :: _ "pager") unit PagerUI.ui state.pager $ Just <<< HandlePager
  , HH.slot (SProxy :: _ "populate") unit PopulateUI.ui populateInput $ Just <<< HandlePopulate
  ]

  where
    populateInput =
      { creater: creater state.client
      , placeholder: Array.intercalate ", " [ Ja.user_name, Ja.user_code, Ja.user_tel ]
      }

    renderHeader =
      HH.tr_
      [ HH.td_
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
      [ HH.td_
        [ renderIsAdmin is_admin ]
      , HH.td_
        [ HH.a
          [ HP.href $ R.path $ R.UsersShow id ]
          [ HH.text name ]
        ]
      , HH.td_
        [ HH.text code ]
      , HH.td_
        [ HH.text tel ]
      , HH.dt_
        [ HH.button
          [ HP.class_ $ H.ClassName "btn btn-outline-danger btn-sm"
          , HE.onClick $ const $ Just $ Destroy id
          ]
          [ HH.i [ HP.class_ $ H.ClassName "fa fa-times" ] []
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


handleAction ::
  forall m.
  MonadAff m =>
  Action -> H.HalogenM State Action ChildSlots Message m Unit
handleAction = case _ of
  SetLocale locale -> do
    H.modify_ _{ locale = locale }

  Reload -> do
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      pager@{ current, per } <- H.gets _.pager
      let offset = (current - 1) * per
      res <- H.liftAff $ attempt $ Users.page cli offset per

      case res of
        Right { body, range: { count } } -> do
          let pager_ = pager { count = count }
          H.modify_ _{ items = body, pager = pager_ }
          void $ H.query (SProxy :: SProxy "pager") unit $ H.tell $ PagerUI.SetPager pager_
        Left _ ->
          H.raise $ Failed "Failed to access api."

  Destroy userId -> do
    Util.whenNotBusy_ do
      cli <- H.gets _.client
      res <- H.liftAff $ attempt $ Users.destroy cli userId

      case res of
        Right _ -> do
          items <- Array.filter ((userId /= _) <<< view User._id) <$> H.gets _.items
          H.modify_ _{ items = items }
        Left _ ->
          H.raise $ Failed "Failed to delete user."

  HandlePager (PagerUI.Selected page) -> do
    pager <- H.gets _.pager
    H.modify_ _{ pager = pager { current = page } }
    handleAction Reload

  HandlePopulate (PopulateUI.Created user) -> do
    items <- H.gets _.items
    H.modify_ _{ items = Array.snoc items user }

  HandlePopulate (PopulateUI.Failed _) -> do
    H.raise $ Failed "Failed to create some users."


creater :: forall m. MonadAff m => Client -> String -> m (Maybe User)
creater cli row =
  maybe (pure Nothing) (map hush <<< H.liftAff <<< attempt <<< Users.create cli) entity

  where
    cols = String.trim <$> String.split (Pattern ",") row
    entity = do
      name <- cols !! 0
      code <- cols !! 1
      tel <- cols !! 2
      pure $ UserEntity { name, code, tel, is_admin: false }
