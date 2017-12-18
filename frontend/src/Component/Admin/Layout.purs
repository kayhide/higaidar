module Component.Admin.Layout where

import Prelude

import Api as Api
import Component.Admin.Route as R
import Component.LoginUI as LoginUI
import Component.NoticeUI as NoticeUI
import Component.PestListUI as PestListUI
import Component.UserListUI as UserListUI
import Component.UserShowUI as UserShowUI
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Now as Now
import Data.DateTime.Locale (Locale(..), LocaleName(..))
import Data.Either.Nested (Either5)
import Data.Functor.Coproduct.Nested (Coproduct5)
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
import Routing (matches)


type AppConfig =
  { stage :: String
  , apiEndpoint :: String
  }

data Query a
  = Initialize a
  | HandleNotice NoticeUI.Message a
  | HandleLogin LoginUI.Message a
  | HandleUserList UserListUI.Message a
  | HandleUserShow UserShowUI.Message a
  | HandlePestList PestListUI.Message a
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

type ChildQuery = Coproduct5 NoticeUI.Query LoginUI.Query UserListUI.Query UserShowUI.Query PestListUI.Query
type ChildSlot = Either5 NoticeUI.Slot LoginUI.Slot UserListUI.Slot UserShowUI.Slot PestListUI.Slot

cpNotice :: CP.ChildPath NoticeUI.Query ChildQuery NoticeUI.Slot ChildSlot
cpNotice = CP.cp1

cpLogin :: CP.ChildPath LoginUI.Query ChildQuery LoginUI.Slot ChildSlot
cpLogin = CP.cp2

cpUserList :: CP.ChildPath UserListUI.Query ChildQuery UserListUI.Slot ChildSlot
cpUserList = CP.cp3

cpUserShow :: CP.ChildPath UserShowUI.Query ChildQuery UserShowUI.Slot ChildSlot
cpUserShow = CP.cp4

cpPestList :: CP.ChildPath PestListUI.Query ChildQuery PestListUI.Slot ChildSlot
cpPestList = CP.cp5

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
    [ HP.class_ $ H.ClassName "navbar navbar-expand-sm navbar-dark bg-dark" ]
    [
      HH.a
      [ HP.class_ $ H.ClassName "navbar-brand mb-0"
      , HP.href "/#/"
      ]
      [ HH.text "Higaidar" ]
    , HH.ul
      [ HP.class_ $ H.ClassName "navbar-nav mr-auto" ]
      [
        HH.li
        [ HP.class_ $ H.ClassName "nav-item" ]
        [
          HH.a
          [ HP.class_ $ H.ClassName "nav-link"
          , HP.href $ R.path $ R.UsersIndex
          ]
          [ HH.text "Users" ]
        ]
      , HH.li
        [ HP.class_ $ H.ClassName "nav-item" ]
        [
          HH.a
          [ HP.class_ $ H.ClassName "nav-link"
          , HP.href $ R.path $ R.PestsIndex
          ]
          [ HH.text "Pests" ]
        ]
      ]
    , renderUserName
    , HH.a
      [ HP.class_ $ H.ClassName $ "btn btn-sm "
        <> if isAuthenticated then "btn-secondary" else "btn-outline-secondary"
      , HP.href $ R.path R.Login
      ]
      [
        HH.i [ HP.class_ $ H.ClassName "fa fa-user fa-fw" ] []
      ]
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

    renderUserName = case client of
      Api.Client { user: Just (User { name }) } ->
        HH.span
        [ HP.class_ $ H.ClassName "navbar-text mr-2" ]
        [ HH.text $ name ]
      _ ->
        HH.span_ []

    renderPage = case _ of
      R.Home -> withAuthentication
                $ HH.p_ [ HH.text $ "Home" ]

      R.Login ->
        HH.slot' cpLogin LoginUI.Slot LoginUI.ui { endpoint } $ HE.input HandleLogin

      R.UsersIndex -> withAuthentication
        $ HH.slot' cpUserList UserListUI.Slot UserListUI.ui { client, locale } $ HE.input HandleUserList

      R.UsersShow userId -> withAuthentication
        $ HH.slot' cpUserShow UserShowUI.Slot UserShowUI.ui { userId, client, locale } $ HE.input HandleUserShow

      R.PestsIndex -> withAuthentication
        $ HH.slot' cpPestList PestListUI.Slot PestListUI.ui { client, locale } $ HE.input HandlePestList

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

  HandleUserList (UserListUI.Failed s) next -> do
    postAlert s
    pure next

  HandleUserShow (UserShowUI.Failed s) next -> do
    postAlert s
    pure next

  HandlePestList (PestListUI.Failed s) next -> do
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
