module Component.Admin.Layout where

import AppPrelude

import Api as Api
import AppConfig (AppConfig)
import Component.Admin.CropListPage as CropListPage
import Component.Admin.PestListPage as PestListPage
import Component.Admin.PhotoListPage as PhotoListPage
import Component.Admin.Route as R
import Component.Admin.UserEditPage as UserEditPage
import Component.Admin.UserListPage as UserListPage
import Component.LoginUI as LoginUI
import Component.NoticeUI as NoticeUI
import Data.String as String
import Data.Time.Duration (Minutes(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import I18n.Ja as Ja
import Model.DateTime (Locale(..), getLocale)
import Model.User (User(..))
import Routing.Hash as Routing


data Action
  = Initialize
  | HandleLogin LoginUI.Message
  | HandleUserList UserListPage.Message
  | HandleUserEdit UserEditPage.Message
  | HandlePhotoList PhotoListPage.Message
  | HandleCropList CropListPage.Message
  | HandlePestList PestListPage.Message

type State =
  { appConfig :: AppConfig
  , apiClient :: Api.Client
  , locale :: Locale
  , location :: R.Location
  }

type Input = AppConfig

data Query a
  = Goto R.Location a

type Message = Void

type ChildSlots =
  ( notice :: H.Slot NoticeUI.Query NoticeUI.Message Unit
  , login :: H.Slot (Const Void) LoginUI.Message Unit
  , userList :: H.Slot (Const Void) UserListPage.Message Unit
  , userEdit :: H.Slot (Const Void) UserEditPage.Message Unit
  , photoList :: H.Slot (Const Void) PhotoListPage.Message Unit
  , cropList :: H.Slot (Const Void) CropListPage.Message Unit
  , pestList :: H.Slot (Const Void) PestListPage.Message Unit
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
    , handleQuery = handleQuery
    , initialize = Just Initialize
    }
  }

  where
    initialState appConfig@({ apiEndpoint }) =
      { appConfig
      , apiClient: Api.makeClient apiEndpoint
      , locale: Locale (Just "GMT") (Minutes 0.0)
      , location: R.Home
      }


render ::
  forall m.
  MonadAff m =>
  State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
  [ renderNavbar
  , HH.slot (SProxy :: _ "notice") unit NoticeUI.ui unit $ const Nothing
  , HH.main
    [ HP.class_ $ H.ClassName "container mt-2 mb-5" ]
    [ renderPage state.location
    ]
  ]

  where
    client = state.apiClient
    locale = state.locale
    isAuthenticated = Api.isAuthenticated client

    renderNavbar =
      HH.nav
      [ HP.class_ $ H.ClassName "navbar navbar-expand navbar-dark bg-dark" ]
      [ HH.a
        [ HP.class_ $ H.ClassName "navbar-brand mb-0 d-none d-sm-block"
        , HP.href "/#/"
        ]
        [ HH.text Ja.higaidar ]
      , HH.a
        [ HP.class_ $ H.ClassName "btn btn-sm btn-outline-secondary mr-3 d-block d-sm-none"
        , HP.href "/#/"
        ]
        [ HH.text $ String.take 1 Ja.higaidar ]
      , HH.ul
        [ HP.class_ $ H.ClassName "navbar-nav mr-auto" ]
        [
          renderMenuItem R.UsersIndex Ja.user "users"
        , renderMenuItem R.PhotosIndex Ja.photo "picture-o"
        , renderMenuItem R.CropsIndex Ja.crop "leaf"
        , renderMenuItem R.PestsIndex Ja.pest "bug"
        ]
      , renderUserName
      , HH.a
        [ HP.class_ $ H.ClassName $ "btn btn-sm "
          <> if isAuthenticated then "btn-secondary" else "btn-outline-secondary"
        , HP.href $ R.path R.Login
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fa fa-user fa-fw" ] []
        ]
      ]

    renderMenuItem location text icon =
      HH.li
      [ HP.class_ $ H.ClassName "nav-item" ]
      [ HH.a
        [ HP.class_ $ H.ClassName "nav-link"
        , HP.href $ R.path location
        ]
        $ renderTextOrIcon text icon
      ]

    renderTextOrIcon text icon =
      [ HH.span
        [ HP.class_ $ H.ClassName "d-none d-sm-inline" ]
        [ HH.text text ]
      , HH.i
        [ HP.class_ $ H.ClassName $ "fa fa-fw fa-" <> icon <> " d-inline d-sm-none" ]
        []
      ]

    renderUserName = case client of
      Api.Client { user: Just (User { name }) } ->
        HH.span
        [ HP.class_ $ H.ClassName "navbar-text mr-2" ]
        [ HH.text $ name ]
      _ ->
        HH.span_ []

    renderPage = case _ of
      R.Home ->
        withAuthentication
        $ HH.ul
        [ HP.class_ $ H.ClassName "nav flex-column" ]
        [ HH.li
          [ HP.class_ $ H.ClassName "nav-item" ]
          [ HH.a
            [ HP.class_ $ H.ClassName "nav-link"
            , HP.href $ R.path $ R.UsersIndex
            ]
            [ HH.i [ HP.class_ $ H.ClassName "fa fa-fw fa-users mr-2" ] []
            , HH.text Ja.user
            ]
          ]
        , HH.li
          [ HP.class_ $ H.ClassName "nav-item" ]
          [ HH.a
            [ HP.class_ $ H.ClassName "nav-link"
            , HP.href $ R.path $ R.PhotosIndex
            ]
            [ HH.i [ HP.class_ $ H.ClassName "fa fa-fw fa-picture-o mr-2" ] []
            , HH.text Ja.photo
            ]
          ]
        , HH.li
          [ HP.class_ $ H.ClassName "nav-item" ]
          [ HH.a
            [ HP.class_ $ H.ClassName "nav-link"
            , HP.href $ R.path $ R.CropsIndex
            ]
            [ HH.i [ HP.class_ $ H.ClassName "fa fa-fw fa-leaf mr-2" ] []
            , HH.text Ja.crop
            ]
          ]
        , HH.li
          [ HP.class_ $ H.ClassName "nav-item" ]
          [ HH.a
            [ HP.class_ $ H.ClassName "nav-link"
            , HP.href $ R.path $ R.PestsIndex
            ]
            [ HH.i [ HP.class_ $ H.ClassName "fa fa-fw fa-bug mr-2" ] []
            , HH.text Ja.pest
            ]
          ]
        ]

      R.Login ->
        HH.slot (SProxy :: _ "login") unit LoginUI.ui client $ Just <<< HandleLogin

      R.UsersIndex ->
        withAuthentication
        -- $ HH.slot' cpUserList UserListPage.Slot UserListPage.ui { client, locale } $ HE.input HandleUserList
        $ HH.slot (SProxy :: _ "userList") unit UserListPage.ui { client, locale } $ Just <<< HandleUserList

      R.UsersShow userId ->
        withAuthentication
        $ HH.slot (SProxy :: _ "userEdit") unit UserEditPage.ui { userId, client, locale } $ Just <<< HandleUserEdit

      R.PhotosIndex ->
        withAuthentication
        $ HH.slot (SProxy :: _ "photoList") unit PhotoListPage.ui { client, locale } $ Just <<< HandlePhotoList

      R.CropsIndex ->
        withAuthentication
        $ HH.slot (SProxy :: _ "cropList") unit CropListPage.ui { client, locale } $ Just <<< HandleCropList

      R.PestsIndex ->
        withAuthentication
        $ HH.slot (SProxy :: _ "pestList") unit PestListPage.ui { client, locale } $ Just <<< HandlePestList

    withAuthentication html =
      if isAuthenticated
      then html
      else HH.slot (SProxy :: _ "login") unit LoginUI.ui client $ Just <<< HandleLogin

handleAction ::
  forall m.
  MonadEffect m =>
  MonadAff m =>
  Action -> H.HalogenM State Action ChildSlots Void m Unit
handleAction = case _ of
  Initialize -> do
    locale <- liftEffect getLocale
    H.modify_ _{ locale = locale }

    hash <- liftEffect Routing.getHash
    when (String.null hash) $
      liftEffect $ Routing.setHash "/"

  HandleLogin (LoginUI.Authenticated client) -> do
    H.modify_ _{ apiClient = client }
    postInfo "Authenticated."

  HandleLogin (LoginUI.Unauthenticated client) -> do
    H.modify_ _{ apiClient = client }
    postInfo "Unauthenticated."

  HandleLogin (LoginUI.Failed client _ s) -> do
    H.modify_ _{ apiClient = client }
    postAlert s

  HandleUserList (UserListPage.Failed s) -> do
    postAlert s

  HandleUserEdit (UserEditPage.Failed s) -> do
    postAlert s

  HandlePhotoList (PhotoListPage.Failed s) -> do
    postAlert s

  HandleCropList (CropListPage.Failed s) -> do
    postAlert s

  HandlePestList (PestListPage.Failed s) -> do
    postAlert s

  where
    postNotice notice =
      void $ H.query (SProxy :: SProxy "notice") unit $ H.tell $ NoticeUI.Post notice
    postInfo s = postNotice $ NoticeUI.Info s
    postAlert s = postNotice $ NoticeUI.Alert s


handleQuery ::
  forall m a.
  Query a -> H.HalogenM State Action ChildSlots Message m (Maybe a)
handleQuery = case _ of
  Goto loc next -> do
    H.modify_ _{ location = loc }
    pure $ Just next
