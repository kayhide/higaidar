module Component.General.Layout where

import Prelude

import Api as Api
import Component.LoginUI as LoginUI
import Component.NoticeUI as NoticeUI
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Now as Now
import Data.DateTime.Locale (Locale(..), LocaleName(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Time.Duration (Minutes(..))
import Dom.Storage (STORAGE)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model.User (User(..))
import Network.HTTP.Affjax (AJAX)
import Route as R
import Routing (matches)


type AppConfig =
  { stage :: String
  , apiEndpoint :: String
  }

data Query a
  = Initialize a
  | HandleNotice NoticeUI.Message a
  | HandleLogin LoginUI.Message a
  | Goto R.Location a

type State =
  { appConfig :: AppConfig
  , apiClient :: Api.Client
  , locale :: Locale
  , location :: R.Location
  , locationAfterLogin :: R.Location
  }

type Input = AppConfig

type Message = Void

type ChildQuery = Coproduct2 NoticeUI.Query LoginUI.Query
type ChildSlot = Either2 NoticeUI.Slot LoginUI.Slot

cpNotice :: CP.ChildPath NoticeUI.Query ChildQuery NoticeUI.Slot ChildSlot
cpNotice = CP.cp1

cpLogin :: CP.ChildPath LoginUI.Query ChildQuery LoginUI.Slot ChildSlot
cpLogin = CP.cp2

type Eff_ eff = Aff (ajax :: AJAX, now :: NOW, storage :: STORAGE | eff)

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
  where
    initialState appConfig@({ apiEndpoint }) =
      { appConfig
      , apiClient: Api.makeClient apiEndpoint
      , locale: Locale (Just (LocaleName "GMT")) (Minutes 0.0)
      , location: R.Home
      , locationAfterLogin: R.Home
      }


render :: forall eff. State -> H.ParentHTML Query ChildQuery ChildSlot (Eff_ eff)
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
      [ HH.text "Higaidar" ]
    , renderAdminMenu
    , renderUserName
    , renderLoginButton
    ]
  , HH.slot' cpNotice NoticeUI.Slot NoticeUI.ui unit $ HE.input HandleNotice
  , HH.main
    [ HP.class_ $ H.ClassName "container mt-2" ]
    [
      renderPage state.location
    ]
  ]

  where
    endpoint = state.appConfig.apiEndpoint
    client = state.apiClient
    locale = state.locale
    isAuthenticated = Api.isAuthenticated client

    updateButtonClass =
      "btn btn-outline-primary mb-2"
      <> if isAuthenticated then "" else " disabled"

    renderAdminMenu = case client of
      Api.Client { user: Just (User { is_admin: true }) } ->
        HH.ul
        [ HP.class_ $ H.ClassName "navbar-nav ml-auto mr-4" ]
        [
          HH.li
          [ HP.class_ $ H.ClassName "nav-item" ]
          [
            HH.a
            [ HP.class_ $ H.ClassName "nav-link"
            , HP.href "/admin/#/"
            ]
            [ HH.text "Admin" ]
          ]
        ]
      _ ->
        HH.ul
        [ HP.class_ $ H.ClassName "navbar-nav ml-auto" ]
        []

    renderUserName = case client of
      Api.Client { user: Just (User { name }) } ->
        HH.span
        [ HP.class_ $ H.ClassName "navbar-text mr-2" ]
        [ HH.text $ name ]
      _ ->
        HH.span_ []

    renderLoginButton =
      HH.a
      [ HP.class_ $ H.ClassName $ "btn btn-sm "
        <> if isAuthenticated then "btn-secondary" else "btn-outline-secondary"
      , HP.href $ R.path R.Login
      ]
      [
        HH.i [ HP.class_ $ H.ClassName "fa fa-user fa-fw" ] []
      ]

    renderPage = case _ of
      R.Login ->
        HH.slot' cpLogin LoginUI.Slot LoginUI.ui { endpoint } $ HE.input HandleLogin

      R.Home ->
        withAuthentication
        $ HH.p_ [ HH.text $ "Home" ]

      _ ->
        HH.text $ "Page not found"

    withAuthentication html =
      if Api.isAuthenticated client
      then html
      else HH.text $ "Not authenticated"

eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    locale <- H.liftEff Now.locale
    H.modify _{ locale = locale }
    eval $ Goto R.Home next

  HandleNotice (NoticeUI.Closed i) next -> do
    pure next

  HandleLogin (LoginUI.Authenticated client) next -> do
    loc <- H.gets _.locationAfterLogin
    H.modify _{ apiClient = client, location = loc }
    postInfo "Authenticated."
    pure next

  HandleLogin (LoginUI.Failed client _ s) next -> do
    H.modify _{ apiClient = client }
    postAlert s
    pure next

  Goto loc next -> do
    cli <- H.gets _.apiClient
    case Api.isAuthenticated cli of
      true ->
        H.modify _{ location = loc, locationAfterLogin = loc }
      false ->
        H.modify _{ location = R.Login, locationAfterLogin = loc }
    pure next

  where
    postNotice notice =
      void $ H.query' cpNotice NoticeUI.Slot $ H.action $ NoticeUI.Post notice
    postInfo s = postNotice $ NoticeUI.Info s
    postAlert s = postNotice $ NoticeUI.Alert s



matchRoute :: forall eff. H.HalogenIO Query Void (Aff (HA.HalogenEffects eff))
              -> Eff (HA.HalogenEffects eff) Unit
matchRoute driver = matches R.routing $ redirects
  where
    redirects _ = launchAff_ <<< driver.query <<< H.action <<< Goto
