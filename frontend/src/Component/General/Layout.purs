module Component.General.Layout where

import AppPrelude

import Api.Client (Client(..), isAdmin, isAuthenticated, isEditor, makeClient)
import AppConfig (AppConfig)
import Component.General.HomePage as HomePage
import Component.General.Route as R
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
  | HandleHome HomePage.Message

type State =
  { appConfig :: AppConfig
  , apiClient :: Client
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
  , home :: H.Slot (Const Void) HomePage.Message Unit
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
      , apiClient: makeClient apiEndpoint
      , locale: Locale (Just "GMT") (Minutes 0.0)
      , location: R.Home
      }


render ::
  forall m.
  MonadAff m =>
  State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
  [
    HH.nav
    [ HP.class_ $ H.ClassName "navbar navbar-light bg-light" ]
    [
      HH.a
      [ HP.class_ $ H.ClassName "navbar-brand mb-0"
      , HP.href $ R.path R.Home
      ]
      [ HH.text Ja.higaidar ]
    , renderAdminMenu
    , renderUserName
    , renderLoginButton
    ]
  , HH.slot (SProxy :: _ "notice") unit NoticeUI.ui unit $ const Nothing
  , HH.main
    [ HP.class_ $ H.ClassName "container mt-2 mb-5" ]
    [ renderPage state.location
    ]
  ]

  where
    client = state.apiClient
    locale = state.locale

    updateButtonClass =
      "btn btn-outline-primary mb-2"
      <> bool " disabled" "" (isAuthenticated client)

    renderAdminMenu = case isAdmin client, isEditor client of
      true, _ ->
        HH.a
        [ HP.class_ $ H.ClassName "badge badge-danger ml-auto mr-2"
        , HP.href "/admin/#/"
        ]
        [ HH.text "admin" ]
      _, true ->
        HH.a
        [ HP.class_ $ H.ClassName "badge badge-success ml-auto mr-2"
        , HP.href "/admin/#/"
        ]
        [ HH.text "editor" ]
      _, _ ->
        HH.ul
        [ HP.class_ $ H.ClassName "navbar-nav ml-auto" ]
        []

    renderUserName = case client of
      Client { user: Just (User { name }) } ->
        HH.span
        [ HP.class_ $ H.ClassName "navbar-text mr-2" ]
        [ HH.text $ name ]
      _ ->
        HH.span_ []

    renderLoginButton =
      HH.a
      [ HP.class_ $ H.ClassName $ "btn btn-sm "
        <> bool "btn-outline-secondary" "btn-secondary" (isAuthenticated client)
      , HP.href $ R.path R.Login
      ]
      [
        HH.i [ HP.class_ $ H.ClassName "fa fa-user fa-fw" ] []
      ]

    renderPage = case _ of
      R.Login ->
        HH.slot (SProxy :: _ "login") unit LoginUI.ui client $ Just <<< HandleLogin

      R.Home ->
        withAuthentication
        $ HH.slot (SProxy :: _ "home") unit HomePage.ui { client, locale } $ Just <<< HandleHome

    withAuthentication html =
      if isAuthenticated client
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

  HandleHome (HomePage.Failed s) -> do
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
