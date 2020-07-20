module Api where

import AppPrelude

import Affjax (Request, Response, defaultRequest, request)
import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RequestFormat
import Affjax.ResponseHeader as ResponseHeader
import Api.Client (Client(..), AuthenticationToken)
import Api.Client as Client
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Effect.Aff (error, throwError)
import Foreign.Object (Object)
import Foreign.Object as Object
import Global (encodeURI)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Language as ParserLanguage
import Text.Parsing.Parser.String as P
import Text.Parsing.Parser.Token (makeTokenParser)


type Range =
  { first :: Int
  , last :: Int
  , count :: Int
  }

type WithRange a =
  { body :: a
  , range :: Range
  }

get :: forall a. DecodeJson a => Client -> String -> Aff a
get cli path
  = buildGet cli path >>= request >>= handleRequestError >>= handle

getWithRange :: forall a. DecodeJson a => Client -> String -> Aff (WithRange a)
getWithRange cli path = do
  res <- buildGet cli path >>= request >>= handleRequestError
  body <- handle res
  range <- pickContentRange res
  pure { body, range }


post :: forall a b. EncodeJson a => DecodeJson b => Client -> String -> a -> Aff b
post cli path x
  = buildPost cli path x >>= request >>= handleRequestError >>= handle

patch :: forall a b. EncodeJson a => DecodeJson b => Client -> String -> a -> Aff b
patch cli path x
  = buildPatch cli path x >>= request >>= handleRequestError >>= handle

delete :: Client -> String -> Aff Unit
delete cli path
  = void $ buildDelete cli path >>= request >>= handleRequestError >>= const (pure unit)


verifyToken :: Client -> Aff AuthenticationToken
verifyToken (Client cli) = maybe throw_ pure cli.token
  where
    throw_ = throwError $ error "Token is not ready."

buildGet :: Client -> String -> Aff (Request Json)
buildGet cli path = do
  token <- verifyToken cli
  let url = cli ^. Client._endpoint <> path
      headers = [ RequestHeader "Authorization" $ "Bearer " <> token ]
  pure $ defaultRequest
    { url = url
    , headers = headers
    , responseFormat = RequestFormat.json
    }

buildPost :: forall a. EncodeJson a => Client -> String -> a -> Aff (Request Json)
buildPost cli path x = do
  req <- buildGet cli path
  pure $ req { method = Left POST, content = Just $ RequestBody.json $ encodeJson x }

buildPatch :: forall a. EncodeJson a => Client -> String -> a -> Aff (Request Json)
buildPatch cli path x = do
  req <- buildPost cli path x
  pure $ req { method = Left PATCH }

buildDelete :: Client -> String -> Aff (Request Json)
buildDelete cli path = do
  req <- buildGet cli path
  pure $ req { method = Left DELETE }


newtype ResponseNg
  = ResponseNg
    { message :: String
    }
derive instance genericErrorResponse :: Generic ResponseNg _
derive instance newtypeErrorResponse :: Newtype ResponseNg _
derive newtype instance decodeErrorResponse :: DecodeJson ResponseNg


handleRequestError ::
  forall a.
  Either Affjax.Error (Response a) -> Aff (Response a)
handleRequestError =
  either (throwError <<< error <<< Affjax.printError) pure

handle ::
  forall a.
  DecodeJson a =>
  Response Json -> Aff a
handle res = case decodeJson res.body of
  Right x -> pure x
  Left _ -> handleError res.body

handleError :: forall a. Json -> Aff a
handleError body =
  case decodeJson body of
    Right (ResponseNg ng) -> throwError $ error ng.message
    Left err -> throwError $ error err

pickContentRange :: Response Json -> Aff Range
pickContentRange res = do
  contentRange <-
    maybe (throwError $ error "Content-Range not present") (pure <<< ResponseHeader.value)
    $ Array.find (eq "content-range" <<< ResponseHeader.name) res.headers
  range <-
    either (const $ throwError $ error "Content-Range mal-formatted") pure
    $ runParser contentRange parser
  pure range
  where
    tokenParser = makeTokenParser ParserLanguage.haskellDef
    parser = do
      first <- tokenParser.integer
      void $ P.char '-'
      last <- tokenParser.integer
      void $ P.char '/'
      count <- tokenParser.integer
      pure { first, last, count }



data Filtering = Eq_ String | In_ (Array String)

type FilteringMap = Object Filtering

querify :: Filtering -> String
querify (Eq_ x) = "eq(" <> (fromMaybe "" $ encodeURI x) <> ")"
querify (In_ xs) = "in(" <> (Array.intercalate "," $ fromMaybe "" <<< encodeURI <$> xs) <> ")"

buildQuery :: FilteringMap -> String
buildQuery = Array.intercalate "&" <<< Object.mapWithKey (\k v -> (fromMaybe "" $ encodeURI k) <> "=" <> querify v)
