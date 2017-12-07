module Dom.Storage where

import Prelude

import Control.Monad.Aff (Aff, attempt, throwError)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, error, try)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Maybe (Maybe(..))

foreign import data STORAGE :: Effect

foreign import _get :: forall eff. String -> Eff (exception :: EXCEPTION | eff) Foreign
foreign import _set :: forall eff. String -> Foreign -> Eff (exception :: EXCEPTION | eff) Unit

get :: forall eff a. Decode a => String -> Aff (storage :: STORAGE | eff) a
get key = do
  val <- liftEff $ try $ _get key
  case val of
    Right val_ -> do
      either (throwError <<< error <<< show) pure $ runExcept $ decode val_
    Left _ -> throwError $ error $ "Value not found: " <> key

getMay :: forall eff a. Decode a => String -> Aff (storage :: STORAGE | eff) (Maybe a)
getMay key =
  either (const Nothing) Just <$> (attempt $ get key)


set :: forall eff a. Encode a => String -> a -> Aff (storage :: STORAGE | eff) Unit
set key val = do
  res <- liftEff $ try $ _set key $ encode val
  either (throwError <<< error <<< show) (const $ pure unit) res
