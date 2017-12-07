module Component.MainUI where

import Prelude

import Api.Token (AuthenticationToken)
import Component.LoginUI as LoginUI
import Component.NoticeUI as NoticeUI
import Component.UsersUI as UsersUI
import Control.Monad.Aff (Aff, attempt, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Now as Now
import Data.DateTime.Locale (Locale(..), LocaleName(..))
import Data.Either (Either(..))
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Time.Duration (Minutes(..))
import Data.Tuple (Tuple(..))
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
  | HandleUsers UsersUI.Message a
  | Goto R.Location a

type State =
  { appConfig :: AppConfig
  , authenticated :: Boolean
  , token :: Maybe AuthenticationToken
  , locale :: Locale
  , location :: R.Location
  }

type Input = AppConfig

type Message = Void

data NoticeSlot = NoticeSlot
derive instance eqNoticeSlot :: Eq NoticeSlot
derive instance ordNoticeSlot :: Ord NoticeSlot

data LoginSlot = LoginSlot
derive instance eqLoginSlot :: Eq LoginSlot
derive instance ordLoginSlot :: Ord LoginSlot

data UsersSlot = UsersSlot
derive instance eqUsersSlot :: Eq UsersSlot
derive instance ordUsersSlot :: Ord UsersSlot

type ChildQuery = Coproduct3 NoticeUI.Query LoginUI.Query UsersUI.Query
type ChildSlot = Either3 NoticeSlot LoginSlot Unit

cpNotice :: CP.ChildPath NoticeUI.Query ChildQuery NoticeSlot ChildSlot
cpNotice = CP.cp1

cpLogin :: CP.ChildPath LoginUI.Query ChildQuery LoginSlot ChildSlot
cpLogin = CP.cp2

cpUsers :: CP.ChildPath UsersUI.Query ChildQuery Unit ChildSlot
cpUsers = CP.cp3

type Eff_ eff = Aff (ajax :: AJAX, now :: NOW, storage :: STORAGE | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleParentComponent
    { initialState: { appConfig: _
                    , authenticated: false
                    , token: Nothing
                    , locale: Locale (Just (LocaleName "GMT")) (Minutes 0.0)
                    , location: R.Home
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
    , HH.slot' cpLogin LoginSlot LoginUI.ui { baseUrl } $ HE.input HandleLogin
    ]
  , HH.slot' cpNotice NoticeSlot NoticeUI.ui unit $ HE.input HandleNotice
  , HH.main
    [ HP.class_ $ H.ClassName "container mt-2" ]
    [
      HH.a [ HP.href $ R.path $ R.UsersIndex ] [ HH.text "Users" ]
    , renderPage state.location
    ]
  ]

  where
    baseUrl = state.appConfig.apiEndpoint
    locale = state.locale

    updateButtonClass =
      "btn btn-outline-primary mb-2" <>
      if state.authenticated
      then ""
      else " disabled"

    renderPage = case _ of
      R.Home ->
        HH.p_ [ HH.text $ "Home" ]

      R.UsersIndex -> case state.token of
        Just token ->
          HH.slot' cpUsers unit UsersUI.ui { baseUrl, token, locale } $ HE.input HandleUsers
        Nothing ->
          HH.text $ "Not authenticated"

      R.UsersShow i ->
        HH.p_ [ HH.text $ "User " <> show i ]

eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    locale <- H.liftEff Now.locale
    H.modify _{ locale = locale }
    user <- H.liftAff $ attempt do
      code <- Storage.get "user.code"
      tel <- Storage.get "user.tel"
      pure $ Tuple code tel

    case user of
      Right (Tuple code tel) -> do
        void $ H.query' cpLogin LoginSlot $ H.action $ LoginUI.SetCode code
        void $ H.query' cpLogin LoginSlot $ H.action $ LoginUI.SetTel tel
        void $ H.query' cpLogin LoginSlot $ H.action $ LoginUI.Authenticate
      Left _ ->
        pure unit

    pure next

  HandleNotice (NoticeUI.Closed i) next -> do
    pure next

  HandleLogin (LoginUI.Authenticated code tel token) next -> do
    H.modify _{ authenticated = true, token = Just token }
    postInfo "Authenticated."
    H.liftAff do
      Storage.set "user.code" code
      Storage.set "user.tel" tel
    pure next

  HandleLogin (LoginUI.Failed s) next -> do
    H.modify _{ authenticated = false }
    postAlert s
    pure next

  HandleUsers (UsersUI.Failed s) next -> do
    postAlert "Failed to access database."
    postAlert "Try login again."
    pure next

  Goto loc next -> do
    H.modify $ _{ location = loc }
    pure next

  where
    postNotice notice =
      void $ H.query' cpNotice NoticeSlot $ H.action $ NoticeUI.Post notice
    postInfo s = postNotice $ NoticeUI.Info s
    postAlert s = postNotice $ NoticeUI.Alert s



matchRoute :: forall eff. H.HalogenIO Query Void (Aff (HA.HalogenEffects eff))
              -> Eff (HA.HalogenEffects eff) Unit
matchRoute driver = matches R.routing $ redirects driver
  where
    redirects driver _ = launchAff_ <<< driver.query <<< H.action <<< Goto
