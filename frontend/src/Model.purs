module Model where

import AppPrelude

import Data.Array as Array
import Data.Lens (Lens', lens)
import Data.Newtype (class Newtype)


atBy ::
  forall a.
  (a -> Boolean) -> Lens' (Array a) (Maybe a)
atBy pred = lens get set
  where
    get :: Array a -> Maybe a
    get = Array.find pred

    set :: Array a -> Maybe a -> Array a
    set xs x = case Array.findIndex pred xs of
        Nothing -> xs <> Array.fromFoldable x
        Just i -> fromMaybe xs $ Array.alterAt i (const x) xs

atOf ::
  forall a b r.
  Newtype b { id :: a | r } =>
  Eq a =>
  a -> Lens' (Array b) (Maybe b)
atOf id' = atBy \ x -> (unwrap x).id == id'
