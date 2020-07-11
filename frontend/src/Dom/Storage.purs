module Dom.Storage where

import AppPrelude

import Control.Monad.Error.Class (throwError, try)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Effect.Exception (error)


foreign import _get :: String -> Effect Json
foreign import _set :: String -> Json -> Effect Unit

get :: forall a m. MonadEffect m => DecodeJson a => String -> m a
get key = liftEffect do
  val <- try $ _get key
  case val of
    Right val_ -> do
      either (throwError <<< error <<< show) pure $ decodeJson val_
    Left _ -> throwError $ error $ "Value not found: " <> key

getMay :: forall a m. MonadEffect m => DecodeJson a => String -> m (Maybe a)
getMay key =
  liftEffect
  $ either (const Nothing) Just <$> (try $ get key)


set :: forall a m. MonadEffect m => EncodeJson a => String -> a -> m Unit
set key val = liftEffect do
  res <- try $ _set key $ encodeJson val
  either (throwError <<< error <<< show) (const $ pure unit) res
