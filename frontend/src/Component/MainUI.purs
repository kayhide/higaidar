module Component.MainUI where

import Prelude

import Api.Token (AuthenticationToken)
import Component.LoginUI as LoginUI
import Component.NoticeUI as NoticeUI
import Component.UserShowUI as UserShowUI
import Component.UserListUI as UserListUI
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Now as Now
import Data.DateTime.Locale (Locale(..), LocaleName(..))
import Data.Either.Nested (Either3, Either4)
import Data.Functor.Coproduct.Nested (Coproduct3, Coproduct4)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Time.Duration (Minutes(..))
import Dom.Storage (STORAGE)
import Dom.Storage as Storage
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
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
  | HandleUserList UserListUI.Message a
  | HandleUserShow UserShowUI.Message a
  | Goto R.Location a

type State =
  { appConfig :: AppConfig
  , token :: Maybe AuthenticationToken
  , userName :: Maybe String
  , locale :: Locale
  , location :: R.Location
  , locationAfterLogin :: R.Location
  }

type Input = AppConfig

type Message = Void

type ChildQuery = Coproduct4 NoticeUI.Query LoginUI.Query UserListUI.Query UserShowUI.Query
type ChildSlot = Either4 NoticeUI.Slot LoginUI.Slot UserListUI.Slot UserShowUI.Slot

cpNotice :: CP.ChildPath NoticeUI.Query ChildQuery NoticeUI.Slot ChildSlot
cpNotice = CP.cp1

cpLogin :: CP.ChildPath LoginUI.Query ChildQuery LoginUI.Slot ChildSlot
cpLogin = CP.cp2

cpUserList :: CP.ChildPath UserListUI.Query ChildQuery UserListUI.Slot ChildSlot
cpUserList = CP.cp3

cpUserShow :: CP.ChildPath UserShowUI.Query ChildQuery UserShowUI.Slot ChildSlot
cpUserShow = CP.cp4

type Eff_ eff = Aff (ajax :: AJAX, now :: NOW, storage :: STORAGE | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleParentComponent
    { initialState: { appConfig: _
                    , token: Nothing
                    , userName: Nothing
                    , locale: Locale (Just (LocaleName "GMT")) (Minutes 0.0)
                    , location: R.Home
                    , locationAfterLogin: R.Home
                    }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

render :: forall eff. State -> H.ParentHTML Query ChildQuery ChildSlot (Eff_ eff)
render state =
  HH.div_
  [
    HH.nav
    [ HP.class_ $ H.ClassName "navbar navbar-dark bg-dark" ]
    [
      HH.a
      [ HP.class_ $ H.ClassName "navbar-brand mb-0"
      , HP.href $ R.path R.Home
      ]
      [ HH.text "Higaidar Admin" ]
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
      ]
    , renderUserName
    , HH.a
      [ HP.class_ $ H.ClassName $ "btn btn-sm " <> maybe "btn-outline-secondary" (const "btn-secondary") state.token
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
    baseUrl = state.appConfig.apiEndpoint
    locale = state.locale

    updateButtonClass =
      "btn btn-outline-primary mb-2" <> maybe " disabled" (const "") state.token

    renderUserName = case state.userName of
      Just userName ->
          HH.span
          [ HP.class_ $ H.ClassName "navbar-text mr-2" ]
          [ HH.text $ userName  ]
      Nothing -> HH.span_ []

    renderPage = case _ of
      R.Home -> withAuthentication \token ->
        HH.p_ [ HH.text $ "Home" ]

      R.Login ->
        HH.slot' cpLogin LoginUI.Slot LoginUI.ui { baseUrl } $ HE.input HandleLogin

      R.UsersIndex -> withAuthentication \token ->
        HH.slot' cpUserList UserListUI.Slot UserListUI.ui { baseUrl, token, locale } $ HE.input HandleUserList

      R.UsersShow userId -> withAuthentication \token ->
        HH.slot' cpUserShow UserShowUI.Slot UserShowUI.ui { userId, baseUrl, token, locale } $ HE.input HandleUserShow

    withAuthentication html = maybe (HH.text $ "Not authenticated") html state.token

eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    locale <- H.liftEff Now.locale
    H.modify _{ locale = locale }
    pure next

  HandleNotice (NoticeUI.Closed i) next -> do
    pure next

  HandleLogin (LoginUI.Authenticated code tel token) next -> do
    loc <- H.gets _.locationAfterLogin
    H.modify _{ token = Just token, userName = Just $ show code, location = loc }
    postInfo "Authenticated."
    H.liftAff do
      Storage.set "user.code" code
      Storage.set "user.tel" tel
    pure next

  HandleLogin (LoginUI.Failed s) next -> do
    H.modify _{ token = Nothing, userName = Nothing }
    postAlert s
    pure next

  HandleUserList (UserListUI.Failed s) next -> do
    H.modify _{ token = Nothing, userName = Nothing }
    postAlert "Failed to access resource."
    postAlert "Try login again."
    pure next

  HandleUserShow (UserShowUI.Failed s) next -> do
    postAlert "Failed to access resource."
    pure next
    -- H.modify _{ token = Nothing, userName = Nothing }
    -- postAlert "Failed to access database."
    -- postAlert "Try login again."
    -- loc <- H.gets _.location
    -- eval $ Goto loc next

  Goto loc next -> do
    token <- H.gets _.token
    case token of
      Just _ ->
        H.modify _{ location = loc, locationAfterLogin = loc }
      Nothing ->
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
