module Component.Admin.UserEditPage where

import AppPrelude

import Api.Client (Client)
import Api.Users as Users
import Component.HTML.Checkbox as Checkbox
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.HTML.TextField as TextField
import Component.Util as Util
import Control.Monad.Except (runExceptT)
import Data.Lens (Lens', _Just, assign, to, view)
import Data.Lens.Record (prop)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n as I18n
import I18n.Ja as Ja
import Model.DateTime (Locale)
import Model.User (User)
import Model.User as User


data Action
  = SetLocale Locale
  | Reload
  | SetString (Lens' User String) String
  | SetBoolean (Lens' User Boolean) Boolean
  | Submit
  | Revert

type State =
  { userId :: Int
  , user :: Maybe User
  , editing :: Maybe User
  , client :: Client
  , locale :: Locale
  , busy :: Boolean
  }

_editing :: Lens' State (Maybe User)
_editing = prop (SProxy :: SProxy "editing")


type Input =
  { userId :: Int
  , client :: Client
  , locale :: Locale
  }

type Query = Const Void

data Message
  = Failed String

type ChildSlots = ()


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
    initialState { userId, client, locale } =
      { userId, client, locale
      , user: Nothing
      , editing: Nothing
      , busy: false
      }

render ::
  forall m.
  MonadAff m =>
  State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
  [
    HH.h1_
    [ HH.text Ja.user ]
  , LoadingIndicator.render state.busy
  , HH.div
    [ HP.class_ $ H.ClassName "row" ]
    [ renderForm ]
  ]

  where
    modified = state.user /= state.editing
    localize = I18n.localizeDateTime state.locale
    created_at = state ^. _editing <<< _Just <<< User._created_at <<< to localize
    updated_at = state ^. _editing <<< _Just <<< User._updated_at <<< to localize

    renderForm =
      HH.div
      [ HP.class_ $ H.ClassName "col-md-12" ]
      [
        renderInput "user-code" Ja.user_code User._code
      , renderInput "user-name" Ja.user_name User._name
      , renderInput "user-tel" Ja.user_tel User._tel
      , renderCheckbox "user-is_admin" Ja.user_admin User._is_admin
      , renderCheckbox "user-is_editor" Ja.user_editor User._is_editor
      , renderStatic "user-created_at" Ja.created_at created_at
      , renderStatic "user-updated_at" Ja.updated_at updated_at
      , HH.hr_
      , HH.div
        [ HP.class_ $ H.ClassName "d-flex mt-2" ]
        [
          renderSubmitButton
        , renderRevertButton
        ]
      ]

    renderInput :: String -> String -> Lens' User String -> _
    renderInput key label attr =
      TextField.render key label (maybe "" (view attr) state.editing) $ SetString attr

    renderCheckbox :: String -> String -> Lens' User Boolean -> _
    renderCheckbox key label attr =
      Checkbox.render key label (maybe false (view attr) state.editing) $ SetBoolean attr

    renderStatic key label value =
      HH.div
      [ HP.class_ $ H.ClassName "form-group" ]
      [
        HH.label
        [ HP.class_ $ H.ClassName "col-form-label"
        , HP.for key
        ]
        [ HH.text label ]
      , HH.input
        [ HP.class_ $ H.ClassName "form-control form-control-plaintext"
        , HP.id_ key
        , HP.value value
        , HP.readOnly true
        ]
      ]

    renderSubmitButton =
      HH.button
      [ HP.class_ $ H.ClassName $ "btn btn-primary" <> if modified then "" else " disabled"
      , HE.onClick $ const $ Just Submit
      ]
      [ HH.text Ja.submit ]

    renderRevertButton =
      HH.button
      [ HP.class_ $ H.ClassName $ "ml-auto btn btn-secondary" <> if modified then "" else " disabled"
      , HE.onClick $ const $ Just Revert
      ]
      [ HH.text Ja.revert ]

handleAction ::
  forall m.
  MonadAff m =>
  Action -> H.HalogenM State Action ChildSlots Message m Unit
handleAction = case _ of
  SetLocale locale -> do
    H.modify_ _{ locale = locale }

  Reload -> do
    Util.whenNotBusy_ do
      { client, userId } <- H.get
      res <- runExceptT do
        Util.onLeft "Failed to load user."
          =<< (H.liftAff $ attempt $ Users.find client userId)

      case res of
        Right user ->
          H.modify_ _{ user = Just user, editing = Just user }
        Left msg ->
          H.raise $ Failed msg

  SetString attr v -> do
    assign (_editing <<< _Just <<< attr) v

  SetBoolean attr v -> do
    assign (_editing <<< _Just <<< attr) v

  Submit -> do
    Util.whenNotBusy_ do
      { client } <- H.get
      res <- runExceptT do
        editing <- Util.onNothing "User not loaded." =<< (H.gets _.editing)
        Util.onLeft "Failed to update user."
          =<< (H.liftAff $ attempt $ Users.update client editing)

      case res of
        Right user ->
          H.modify_ _{ user = Just user, editing = Just user }
        Left msg ->
          H.raise $ Failed msg

  Revert -> do
    H.gets _.user >>= case _ of
      Just user -> do
        H.modify_ _{ editing = Just user }
      Nothing ->
        H.raise $ Failed "User not loaded."
