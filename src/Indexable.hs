module Indexable where

import Geometry

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B


class Indexable t k a | t k -> a
  where
    index :: t -> k -> Maybe a
    index t i
      = Just (unsafeIndex t i)

    unsafeIndex :: t -> k -> a
    unsafeIndex t i
      = case index t i of
          ~(Just a) -> a

    focus :: t -> k -> t
    focus = const

(!) = unsafeIndex


data SomeIndexable k a where
  Ix :: Indexable x k a => x -> SomeIndexable k a

instance Indexable (SomeIndexable k a) k a
  where
    index       (Ix t) i = index       t i
    unsafeIndex (Ix t) i = unsafeIndex t i
    focus       (Ix t) i = Ix (focus t i)


--------

instance Indexable B.ByteString Int Char
  where
    unsafeIndex
      = (B.w2c .) . B.unsafeIndex

    index b i
      | i <  0          = Nothing
      | i >= B.length b = Nothing
      | let             = Just $ unsafeIndex b i

