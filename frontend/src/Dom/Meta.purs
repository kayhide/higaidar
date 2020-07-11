module Dom.Meta where

import AppPrelude

import Control.Monad.Error.Class (throwError, try)
import Effect.Exception (error)


foreign import _get :: String -> Effect String

get :: String -> Effect String
get name = do
  val <- try $ _get name
  case val of
    Right val_ -> pure val_
    Left _ -> throwError $ error $ "Meta not found: " <> name

getMay :: String -> Effect (Maybe String)
getMay name = do
  val <- try $ _get name
  pure $ case val of
    Right val_ -> Just val_
    Left _ -> Nothing
