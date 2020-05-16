module Zipper where

import Indexable
import Geometry
import Stack

import Control.Comonad
import Data.Stream.Infinite as S


data ListZipper a
  = LZ' (Stream a) (Stream a)
  deriving
    (Functor, Foldable, Traversable)

instance Comonad ListZipper where
  extract (LZ _ x _)
    = x

  duplicate
    = genericMove leftLZ rightLZ

deriving instance ComonadApply ListZipper

instance Applicative ListZipper
  where
    pure a
      = LZ' (pure a) (pure a)

    LZ' fl  fr <*> LZ' xl xr
      =  LZ' (fl<*>xl) (fr<*>xr)


pattern LZ ls r rs
  = LZ' ls (r :> rs)

leftLZ (LZ (l:>ls) x rs)
  = LZ ls l (x:>rs)

rightLZ (LZ ls x (r:>rs))
  = LZ (x:>ls) r rs

writeLZ x (LZ ls _ rs)
  = LZ ls x rs


genericMove a b z =
   LZ (iterate' a z) z (iterate' b z)
 where
   iterate' f =
     S.tail . S.iterate f

---------

newtype Z a = Z { unZ :: ListZipper (ListZipper a) }
  deriving
    (Functor, Foldable, Traversable)

instance Comonad Z where
  extract =
    extract . extract . unZ

  duplicate z =
    Z $ fmap horizontal $ vertical z


up :: Z a -> Z a
up (Z z) = Z (leftLZ z)

down :: Z a -> Z a
down (Z z) = Z (rightLZ z)

left :: Z a -> Z a
left (Z z) = Z (fmap leftLZ z)

right :: Z a -> Z a
right (Z z) = Z (fmap rightLZ z)

zWrite :: a -> Z a -> Z a
zWrite x (Z z) =
  Z $ writeLZ newLine z
    where
      newLine = writeLZ x oldLine
      oldLine = extract z

horizontal :: Z a -> ListZipper (Z a)
horizontal =
  genericMove left right

vertical :: Z a -> ListZipper (Z a)
vertical =
  genericMove up down

---------

instance Indexable (Stream a) Int a
  where
    unsafeIndex s i
      = extract $ focus s i

    focus
      = flip S.drop

instance Indexable (ListZipper a) Int a
  where
    unsafeIndex (LZ l x r) i
      | 0 <- i = x
      | i < 0  = unsafeIndex l (abs i - 1)
      | let    = unsafeIndex r (i - 1)

    focus z i
      | 0 <- i = z
      | i < 0  = S.iterate (leftLZ ) z S.!! (abs i)
      | i > 0  = S.iterate (rightLZ) z S.!! i

instance Indexable (Z a) Pos a
  where
    unsafeIndex (Z z) (Pos x y)
      = z ! y ! x

    focus (Z z) (Pos x y)
      = coerce do flip focus x <$> focus z y

