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
import DOM (DOM)
import Data.DateTime.Locale (Locale(..), LocaleName(..))
import Data.Either.Nested (Either5)
import Data.Functor.Coproduct.Nested (Coproduct5)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String as String
import Data.Time.Duration (Minutes(..))
import Dom.Storage (STORAGE)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n.Ja as Ja
import Model.User (User(..))
import Network.HTTP.Affjax (AJAX)
import Routing (matches)
import Routing.Hash as Routing


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

type Eff_ eff = Aff (ajax :: AJAX, dom :: DOM, now :: NOW, storage :: STORAGE | eff)

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
      }


render :: forall eff. State -> H.ParentHTML Query ChildQuery ChildSlot (Eff_ eff)
render state =
  HH.div_
  [
    renderNavbar
  , HH.slot' cpNotice NoticeUI.Slot NoticeUI.ui unit $ HE.input HandleNotice
  , HH.main
    [ HP.class_ $ H.ClassName "container mt-2" ]
    [
      renderPage state.location
    ]
  ]

  where
    client = state.apiClient
    locale = state.locale
    isAuthenticated = Api.isAuthenticated client

    renderNavbar =
      HH.nav
      [ HP.class_ $ H.ClassName "navbar navbar-expand navbar-dark bg-dark" ]
      [
        HH.a
        [ HP.class_ $ H.ClassName "navbar-brand mb-0"
        , HP.href "/#/"
        ]
        [ HH.text Ja.higaidar ]
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
            [ HH.text Ja.user ]
          ]
        , HH.li
          [ HP.class_ $ H.ClassName "nav-item" ]
          [
            HH.a
            [ HP.class_ $ H.ClassName "nav-link"
            , HP.href $ R.path $ R.PestsIndex
            ]
            [ HH.text Ja.pest ]
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
        [
          HH.li
          [ HP.class_ $ H.ClassName "nav-item" ]
          [
            HH.a
            [ HP.class_ $ H.ClassName "nav-link"
            , HP.href $ R.path $ R.UsersIndex
            ]
            [
              HH.i [ HP.class_ $ H.ClassName "fa fa-fw fa-users mr-2" ] []
            , HH.text Ja.user
            ]
          ]
        , HH.li
          [ HP.class_ $ H.ClassName "nav-item" ]
          [
            HH.a
            [ HP.class_ $ H.ClassName "nav-link"
            , HP.href $ R.path $ R.PestsIndex
            ]
            [
              HH.i [ HP.class_ $ H.ClassName "fa fa-fw fa-bug mr-2" ] []
            , HH.text Ja.pest
            ]
          ]
        ]

      R.Login ->
        HH.slot' cpLogin LoginUI.Slot LoginUI.ui client $ HE.input HandleLogin

      R.UsersIndex ->
        withAuthentication
        $ HH.slot' cpUserList UserListUI.Slot UserListUI.ui { client, locale } $ HE.input HandleUserList

      R.UsersShow userId ->
        withAuthentication
        $ HH.slot' cpUserShow UserShowUI.Slot UserShowUI.ui { userId, client, locale } $ HE.input HandleUserShow

      R.PestsIndex ->
        withAuthentication
        $ HH.slot' cpPestList PestListUI.Slot PestListUI.ui { client, locale } $ HE.input HandlePestList

    withAuthentication html =
      if isAuthenticated
      then html
      else
        HH.slot' cpLogin LoginUI.Slot LoginUI.ui client $ HE.input HandleLogin

eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    locale <- H.liftEff Now.locale
    H.modify _{ locale = locale }

    hash <- H.liftEff $ Routing.getHash
    when (String.null hash) $
      H.liftEff $ Routing.setHash "/"

    pure next

  HandleNotice (NoticeUI.Closed i) next -> do
    pure next

  HandleLogin (LoginUI.Authenticated client) next -> do
    H.modify _{ apiClient = client }
    postInfo "Authenticated."
    pure next

  HandleLogin (LoginUI.Unauthenticated client) next -> do
    H.modify _{ apiClient = client }
    postInfo "Unauthenticated."
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
    H.modify _{ location = loc }
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
