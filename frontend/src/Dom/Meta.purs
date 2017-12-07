module Dom.Meta where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException, try)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

foreign import data META :: Effect

foreign import _get :: forall eff. String -> Eff (exception :: EXCEPTION | eff) String

get :: forall eff. String -> Eff (meta :: META, exception :: EXCEPTION | eff) String
get name = do
  val <- try $ _get name
  case val of
    Right val_ -> pure val_
    Left _ -> throwException $ error $ "Meta not found: " <> name

getMay :: forall eff. String -> Eff (meta :: META | eff) (Maybe String)
getMay name = do
  val <- try $ _get name
  pure $ case val of
    Right val_ -> Just val_
    Left _ -> Nothing
