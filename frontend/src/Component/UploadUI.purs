module Component.UploadUI where

import Prelude

import Api as Api
import Api.Photos.SignedUrl as PhotosSigneUrl
import Component.HTML.LoadingIndicator as LoadingIndicator
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import DOM.Event.Event (Event)
import DOM.Event.Event as Event
import DOM.File.File as File
import DOM.File.FileList as FileList
import DOM.File.Types (fileToBlob)
import DOM.HTML.HTMLInputElement as DOM
import DOM.HTML.Indexed.InputType as InputType
import Data.Array as Array
import Data.Either (Either(..), either, hush)
import Data.Lens (set)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.MediaType (MediaType(..))
import Data.URI.URI (_fragment, _query)
import Data.URI.URI as URI
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
  , urls :: Array URL
  }

data Query a
  = Initialize a
  | SetFile Event a

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
                    , urls: []
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
  , renderUploadButton
  , HH.div_ $ renderImage <$> state.urls
  ]

  where
    buttonClass =
      "btn " <> if state.busy then "btn-secondary" else "btn-outline-secondary"

    renderUploadButton =
      HH.label_
      [
        HH.input
        [ HP.class_ $ H.ClassName "d-none"
        , HP.type_ InputType.InputFile
        , HP.accept $ MediaType "image/*"
        , HE.onChange $ HE.input SetFile
        ]
      , HH.span
        [ HP.class_ $ H.ClassName "btn btn-success rounded-circle" ]
        [
          HH.i [ HP.class_ $ H.ClassName "fa fa-plus" ] []
        ]
      ]

    renderImage url =
      HH.img
      [ HP.class_ $ H.ClassName "img-fluid"
      , HP.src url
      ]


eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    pure next

  SetFile e next -> do
    let elm = unsafeCoerce $ Event.target e
    let files = unsafePerformEff $ DOM.files elm
    let file = join $ FileList.item 0 <$> files

    whenNotBusy do
      res <- runExceptT do
        blob <- maybe (throwError "File not set") (pure <<< fileToBlob) file
        filename <- maybe (throwError "File not set") (pure <<< File.name) file
        signed_url <- getSignedUrl filename
        void $ onLeft "Failed to upload"
          =<< (H.liftAff $ attempt $ Affjax.put_ signed_url blob)
        pure signed_url

      case res of
        Right signed_url -> do
          let uri = URI.parse signed_url
              url = URI.print <<< set _query Nothing <<< set _fragment Nothing <$> hush uri
          case url of
            Just url_ -> do
              urls <- Array.cons url_ <$> H.gets _.urls
              H.modify _{ urls = urls }
              H.raise $ Uploaded url_
            Nothing ->
              H.raise $ Failed "Bad url"
        Left msg ->
          H.raise $ Failed msg

    pure next


getSignedUrl :: forall eff. String -> ExceptT String (H.ComponentDSL State Query Message (Eff_ eff)) URL
getSignedUrl filename = do
    cli <- lift $ H.gets _.config.client
    onLeft "Failed to issue signed url"
      =<< (H.liftAff $ attempt $ PhotosSigneUrl.create cli filename)

onLeft :: forall e m. Monad m => String -> Either e ~> ExceptT String m
onLeft s = either (throwError <<< const s) pure

whenNotBusy :: forall eff. H.ComponentDSL State Query Message (Eff_ eff) Unit -> H.ComponentDSL State Query Message (Eff_ eff) Unit
whenNotBusy action = do
  busy <- H.gets _.busy
  when (not busy) do
    H.modify _{ busy = true }
    action
    H.modify _{ busy = false }
  pure unit
