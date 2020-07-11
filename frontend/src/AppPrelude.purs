module AppPrelude
       ( module Prelude
       , module Control.Alt
       , module Control.Monad.Reader
       , module Control.MonadZero
       , module Data.Bifunctor
       , module Data.Const
       , module Data.Either
       , module Data.Function
       , module Data.Lens
       , module Data.Maybe
       , module Data.Newtype
       , module Data.Symbol
       , module Data.Traversable
       , module Data.Tuple
       , module Data.Tuple.Nested
       , module Debug.Trace
       , module Effect
       , module Effect.Aff
       , module Effect.Aff.Class
       , module Effect.Class
       , bool
       , mmap
       , throwOnLeft
       , throwOnNothing
       ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (class MonadAsk)
import Control.MonadZero (class Plus, empty, guard)
import Data.Bifunctor (bimap, lmap, rmap)
import Data.Const (Const(..))
import Data.Either (Either(..), either, hush, isLeft, isRight, note)
import Data.Function (on)
import Data.Lens ((%~), (.~), (^.), (^..), (^?))
import Data.Maybe (Maybe(..), maybe, isNothing, isJust, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, for_, traverse_, traverse)
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), attempt, delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)

bool :: forall a. a -> a -> Boolean -> a
bool x y b = if b then y else x

mmap :: forall a. Monoid a => Eq a => (a -> a) -> a -> a
mmap f = bool f identity =<< (_ == mempty)

throwOnNothing :: forall a m. MonadEffect m => String -> Maybe a -> m a
throwOnNothing msg = throwOnLeft <<< note msg

throwOnLeft :: forall a m. MonadEffect m => Either String a -> m a
throwOnLeft = either (liftEffect <<< throw) pure
