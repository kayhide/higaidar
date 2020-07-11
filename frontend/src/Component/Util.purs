module Component.Util where

import AppPrelude

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State (class MonadState, gets, modify_)


onNothing :: forall m. Monad m => String -> Maybe ~> ExceptT String m
onNothing s = maybe (throwError s) pure

exceptNothing :: forall m. Monad m => Maybe ~> ExceptT String m
exceptNothing = maybe (throwError "Except Nothing") pure


onLeft :: forall e m. Monad m => String -> Either e ~> ExceptT String m
onLeft s = either (throwError <<< const s) pure

exceptLeft :: forall e m. Monad m => Either e ~> ExceptT String m
exceptLeft = either (throwError <<< const "Except Left") pure


type BusyRec r = { busy :: Boolean | r }

whenNotBusy :: forall m r a. Monad m => MonadState (BusyRec r) m => m a -> m (Maybe a)
whenNotBusy action = do
  busy <- gets _.busy
  case busy of
    true ->
      pure Nothing
    false -> do
      modify_ _{ busy = true }
      x <- action
      modify_ _{ busy = false }
      pure $ Just x

whenNotBusy_ :: forall m r a. Monad m => MonadState (BusyRec r) m => m a -> m Unit
whenNotBusy_ = void <<< whenNotBusy
