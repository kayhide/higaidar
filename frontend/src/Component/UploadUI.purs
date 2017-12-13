module Component.UploadUI where

import Prelude

import Api as Api
import Api.Photos.SignedUrl as PhotosSigneUrl
import Component.HTML.LoadingIndicator as LoadingIndicator
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import DOM.Event.Event (Event)
import DOM.Event.Event as Event
import DOM.File.File as File
import DOM.File.FileList as FileList
import DOM.File.FileReader as FileReader
import DOM.File.Types (fileToBlob)
import DOM.HTML.HTMLInputElement as DOM
import DOM.HTML.Indexed.FormMethod (FormMethod(..))
import DOM.HTML.Indexed.InputType as InputType
import DOM.HTML.Types (HTMLInputElement)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.MediaType (MediaType(..))
import Data.MediaType.Common as MediaType
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX, URL)
import Network.HTTP.Affjax as Affjax
import Unsafe.Coerce (unsafeCoerce)


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


type Config =
  { client :: Api.Client
  }

type State =
  { config :: Config
  , busy :: Boolean
  , filename :: Maybe String
  , url :: Maybe URL
  , fields :: StrMap String
  , signed_url :: Maybe URL
  , image_data_url :: Maybe URL
  , upload_result :: Maybe String
  }

data Query a
  = Initialize a
  | SetFile Event a
  | Submit a

type Input = Config

data Message
  = Uploaded URL
  | Failed String


type Eff_ eff = Aff (ajax :: AJAX | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleComponent
    { initialState: { config: _
                    , busy: false
                    , filename: Nothing
                    , url: Nothing
                    , fields: StrMap.empty
                    , signed_url: Nothing
                    , image_data_url: Nothing
                    , upload_result: Nothing
                    }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

render :: State -> H.ComponentHTML Query
render state =
  HH.div_
  [
    HH.h1_
    [ HH.text "Upload" ]
  , LoadingIndicator.render state.busy
  , renderForm
  ]

  where
    buttonClass =
      "btn " <> if state.busy then "btn-secondary" else "btn-outline-secondary"

    renderForm = case state.url of
      Just url ->
        HH.form
        [ HP.class_ $ H.ClassName "form"
        , HP.method POST
        , HP.action url
        , HP.enctype MediaType.multipartFormData
        ]
        $ formFields
        <> [
          HH.input
          [ HP.class_ $ H.ClassName "form-control mr-2"
          , HP.type_ InputType.InputFile
          , HP.name "filename"
          , HP.accept $ MediaType "image/*"
          -- , HE.onValueInput $ HE.input SetFile
          , HE.onChange $ HE.input SetFile
          ]
        , HH.p_ [ HH.text $ fromMaybe "" state.filename ]
        , HH.p_ [ HH.text $ fromMaybe "" state.signed_url ]
        , HH.p_ [ HH.text $ fromMaybe "" state.url ]
        , HH.button
          [ HP.class_ $ H.ClassName "btn btn-primary"
          ]
          [
            HH.i [ HP.class_ $ H.ClassName "fa fa-sign-in fa-fw mr-1" ] []
          , HH.text "Upload"
          ]
        , HH.img
          [ HP.class_ $ H.ClassName "img-fluid"
          , HP.src $ fromMaybe "" state.image_data_url
          ]
        ]
      Nothing ->
        HH.div
        [ HP.class_ $ H.ClassName "form"
        ]
        [
          HH.button
          [ HP.class_ $ H.ClassName $ buttonClass
          , HE.onClick $ HE.input_ Submit
          ]
          [
            HH.i [ HP.class_ $ H.ClassName "fa fa-sign-in fa-fw mr-1" ] []
          , HH.text "Upload"
          ]
        ]

    formFields = StrMap.toArrayWithKey renderInput state.fields

    renderInput key value =
      HH.input
      [
        -- HP.type_ InputHidden
        HP.type_ InputType.InputText
      , HP.name key
      , HP.value value
      ]


eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    H.modify _{ url = Just "#" }
    pure next

  SetFile e next -> do
    let _ = unsafePerformEff $ log "hoge"
    let node = Event.target e
    let elm = unsafeCoerce node :: HTMLInputElement
    let files = unsafePerformEff $ DOM.files elm
    let file = join $ FileList.item 0 <$> files
    case file of
      Just file_ -> do
        H.modify _{ filename = Just $ File.name file_ }
        -- let blob = fileToBlob file_
        -- let reader = unsafePerformEff $ FileReader.fileReader
        -- let _ = unsafePerformEff $ FileReader.readAsDataURL blob reader
        -- let res = FileReader.result reader
        -- pure unit

      Nothing ->
        pure unit

    next' <- eval $ Submit next
    res <- runExceptT do
      signed_url <- maybe (throwError "Signed url not ready") pure =<< H.gets _.signed_url
      blob <- fileToBlob <$> maybe (throwError "File not set") pure file
      onLeft "Failed to access api"
        =<< (H.liftAff $ attempt $ Affjax.put signed_url blob)

    case res of
      Right { status, headers, response } -> do
        H.modify _{ upload_result = Just response }
        pure unit
      Left _ -> pure unit

    pure next'

  Submit next -> do
    busy <- H.gets _.busy
    when (not busy) do
      H.modify _{ busy = true }
      cli <- H.gets _.config.client
      -- res <- runExceptT do
      --   onLeft "Failed to access api"
      --     =<< (H.liftAff $ attempt $ PhotosPresignedPost.create cli "someting.jpg")

      -- case res of
      --   Right { url, fields } ->
      --     H.modify _{ url = Just url, fields = fields }
      --   Left s ->
      --     H.raise $ Failed s

      res <- runExceptT do
        filename <- maybe (throwError "File not set") pure =<< H.gets _.filename
        onLeft "Failed to access api"
          =<< (H.liftAff $ attempt $ PhotosSigneUrl.create cli filename)

      case res of
        Right url ->
          H.modify _{ signed_url = Just url }
        Left s ->
          H.raise $ Failed s

      H.modify _{ busy = false }
    pure next


onLeft :: forall e m. Monad m => String -> Either e ~> ExceptT String m
onLeft s = either (throwError <<< const s) pure
