module Component.Util where

import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State (class MonadState, gets, modify)
import Data.Either (Either, either)
import Data.Maybe (Maybe(Just, Nothing))


onLeft :: forall e m. Monad m => String -> Either e ~> ExceptT String m
onLeft s = either (throwError <<< const s) pure


type BusyRec r = { busy :: Boolean | r }

whenNotBusy :: forall m r a. Monad m => MonadState (BusyRec r) m => m a -> m (Maybe a)
whenNotBusy action = do
  busy <- gets _.busy
  case busy of
    true ->
      pure Nothing
    false -> do
      modify _{ busy = true }
      x <- action
      modify _{ busy = false }
      pure $ Just x

whenNotBusy_ :: forall m r a. Monad m => MonadState (BusyRec r) m => m a -> m Unit
whenNotBusy_ = void <<< whenNotBusy
