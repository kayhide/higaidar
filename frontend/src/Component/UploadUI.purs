module Component.UploadUI where

import AppPrelude

import Affjax (URL)
import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Api as Api
import Api.Photos.SignedUrl as PhotosSigneUrl
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.Util as Util
import Control.Monad.Except (runExceptT, throwError)
import DOM.HTML.Indexed.InputAcceptType (InputAcceptTypeAtom(..))
import Data.Lens (_Just)
import Data.MediaType (MediaType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n.Ja as Ja
import Text.Parsing.Parser (runParser)
import URI as URI
import URI.AbsoluteURI (AbsoluteURIOptions, _query)
import URI.AbsoluteURI as AbsoluteURI
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.File.File as File
import Web.File.FileList as FileList
import Web.HTML.HTMLInputElement as HTMLInputElement


type Config =
  { client :: Api.Client
  }

data Action
  = SetFile Event

type State =
  { config :: Config
  , busy :: Boolean
  }

type Input = Config

type Query = Const Void

data Message
  = Uploaded URL
  | Failed String


ui ::
  forall m.
  MonadAff m =>
  H.Component HH.HTML Query Input Message m
ui =
  H.mkComponent
  { initialState
  , render
  , eval:
    H.mkEval
    $ H.defaultEval
    { handleAction = handleAction
    }
  }

  where
    initialState config =
      { config
      , busy: false
      }

render ::
  forall m.
  MonadEffect m =>
  State -> H.ComponentHTML Action () m
render state =
  HH.div_
  [ LoadingIndicator.render state.busy
  , renderUploadButton
  ]

  where
    renderUploadButton = case state.busy of
      false ->
        HH.label_
        [ HH.input
          [ HP.class_ $ H.ClassName "d-none"
          , HP.type_ $ HP.InputFile
          , HP.accept $ HP.InputAcceptType
            [ AcceptMediaType $ MediaType "image/*" ]
          , HE.onChange $ Just <<< SetFile
          ]
        , HH.span
          [ HP.class_ $ H.ClassName "btn btn-success" ]
          [ HH.text Ja.take_photo
          ]
        ]
      true ->
        HH.div_
        [ HH.span
          [ HP.class_ $ H.ClassName "btn btn-success disabled" ]
          [ HH.text Ja.take_photo
          ]
        ]

handleAction ::
  forall m.
  MonadAff m =>
  Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  SetFile e -> do
    let elm = HTMLInputElement.fromEventTarget =<< Event.target e
    files <- liftEffect $ traverse HTMLInputElement.files elm
    let file = join $ FileList.item 0 <$> join files

    Util.whenNotBusy_ do
      res <- runExceptT do
        blob <- maybe (throwError "File not set") (pure <<< File.toBlob) file
        filename <- maybe (throwError "File not set") (pure <<< File.name) file
        cli <- H.gets _.config.client
        signed_url <- Util.onLeft "Failed to issue signed url"
                      =<< (H.liftAff $ attempt $ PhotosSigneUrl.create cli filename)
        void $ Util.onLeft "Failed to upload"
          =<< (H.liftAff $ attempt $ Affjax.put_ signed_url $ Just $ RequestBody.blob blob)
        pure signed_url

      case res of
        Right signed_url -> do
          let uri = hush $ runParser (signed_url :: String) (AbsoluteURI.parser absoluteURIOptions)
              url = AbsoluteURI.print absoluteURIOptions <$> (uri # _Just <<< _query .~ Nothing)
          H.raise $ maybe (Failed "Bad url") Uploaded url
        Left msg ->
          H.raise $ Failed msg


absoluteURIOptions ∷ Record (AbsoluteURIOptions URI.UserInfo (HostPortPair URI.Host URI.Port) URI.Path URI.HierPath URI.Query)
absoluteURIOptions =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseQuery: pure
  , printQuery: identity
  }
