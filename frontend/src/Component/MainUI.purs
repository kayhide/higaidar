module Component.MainUI where

import Prelude

import Component.LoginUI as LoginUI
import Component.NoticeUI as NoticeUI
import Component.UsersUI as UsersUI
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Now as Now
import Data.DateTime.Locale (Locale(..), LocaleName(..))
import Data.Either (Either(..))
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Minutes(..))
import Data.Tuple (Tuple(..))
import Dom.Storage (STORAGE)
import Dom.Storage as Storage
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)


type AppConfig =
  { stage :: String
  , apiEndpoint :: String
  }

data Query a
  = Initialize a
  | HandleNotice NoticeUI.Message a
  | HandleLogin LoginUI.Message a
  | HandleUsers UsersUI.Message a
  | RequestScanUsers a

type State =
  { appConfig :: AppConfig
  , photosCount :: Maybe Int
  , authenticated :: Boolean
  , locale :: Locale
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
                    , photosCount: Nothing
                    , authenticated: false
                    , locale: Locale (Just (LocaleName "GMT")) (Minutes 0.0)
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
      HH.span
      [ HP.class_ $ H.ClassName "navbar-brand mb-0" ]
      [ HH.text "Higaidar Admin" ]
    , HH.slot' cpLogin LoginSlot LoginUI.ui loginConfig $ HE.input HandleLogin
    ]
  , HH.slot' cpNotice NoticeSlot NoticeUI.ui unit $ HE.input HandleNotice
  , HH.main
    [ HP.class_ $ H.ClassName "container mt-2" ]
    [
      HH.h1_
      [ HH.text "User List" ]
    , HH.p_
      [ HH.text photosCount_ ]
    , HH.button
      [ HP.class_ $ H.ClassName updateButtonClass
      , HP.title "Update"
      , HE.onClick (HE.input_ RequestScanUsers)
      ]
      [ HH.text "Update" ]
    , renderUsers state.authenticated state.locale
    ]
  ]

  where
    photosCount_ = maybe "(unknown)" show state.photosCount
    loginConfig =
      { baseUrl: state.appConfig.apiEndpoint
      }

    updateButtonClass =
      "btn btn-outline-primary mb-2" <>
      if state.authenticated
      then ""
      else " disabled"

    renderUsers false _ = HH.div_ []
    renderUsers true locale =
      HH.slot' cpUsers unit UsersUI.ui uiInput $ HE.input HandleUsers
      where
        uiInput =
          { locale
          , baseUrl: state.appConfig.apiEndpoint
          }

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
    H.modify _{ authenticated = true }
    postInfo "Authenticated."
    H.liftAff do
      Storage.set "user.code" code
      Storage.set "user.tel" tel
    void $ H.query' cpUsers unit $ H.action $ UsersUI.SetToken token
    eval $ RequestScanUsers next

  HandleLogin (LoginUI.Failed s) next -> do
    postAlert s
    H.modify _{ authenticated = false }
    pure next

  HandleUsers (UsersUI.Failed s) next -> do
    postAlert "Failed to access database."
    postAlert "Try login again."
    pure next

  RequestScanUsers next -> do
    void $ H.query' cpUsers unit $ H.action UsersUI.Scan
    pure next


  where
    postNotice notice =
      void $ H.query' cpNotice NoticeSlot $ H.action $ NoticeUI.Post notice
    postInfo s = postNotice $ NoticeUI.Info s
    postAlert s = postNotice $ NoticeUI.Alert s
