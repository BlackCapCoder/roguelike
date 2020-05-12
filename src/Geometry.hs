module Geometry
  ( module Geometry
  , module Data.Semiring
  )
  where

import Data.Semiring
import Data.Euclidean
import Data.Star
import Data.Group


data V2 a = V2 a a
  deriving
    ( Eq, Ord, Show
    , Functor, Foldable, Traversable
    )

instance Applicative V2
  where
    pure = join V2

    V2 fl fr <*> V2 xl xr
      = V2 (fl xl) (fr xr)

instance Semigroup a => Semigroup (V2 a)
  where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (V2 a)
  where
    mempty = pure mempty

instance Group a => Group (V2 a)
  where
    invert = fmap invert
    pow    = traverse pow

instance Abelian a => Abelian (V2 a)

instance Semiring a => Semiring (V2 a)
  where
    plus  = liftA2 plus
    times = liftA2 times
    zero  = pure zero
    one   = pure one

instance Ring a => Ring (V2 a)
  where
    negate = fmap negate

instance (GcdDomain a, Eq a) => GcdDomain (V2 a)
  where
    divide = fmap sequenceA . liftA2 divide
    gcd    = liftA2 gcd

instance (Euclidean a, Eq a) => Euclidean (V2 a)
  where
    quotRem a b
      = let V2 (q1,r1) (q2,r2) = liftA2 quotRem a b
         in (V2 q1 q2, V2 r1 r2)

    degree
      = maximum . fmap degree

instance (Field a, Eq a) => Field (V2 a)

instance Star a => Star (V2 a)
  where
    star  = fmap star
    aplus = fmap aplus





type Size = V2 Int
type Pos  = V2 Int
type Ix   = Int
type Rect = V2 Pos

pattern Pos x y
  = V2 x y :: Pos

pattern Size x y
  = V2 x y :: Size

pattern Rect p s
  = V2 p s :: Rect

pattern Rect' x y w h
  = Rect (Pos x y) (Size w h) :: Rect

pattern N  = V2   0 (-1)
pattern E  = V2   1   0
pattern S  = V2   0   1
pattern W  = V2 (-1)  0
pattern NE = V2   1  (-1)
pattern NW = V2 (-1) (-1)
pattern SE = V2   1    1
pattern SW = V2 (-1)   1

pattern Pos'  x y <- (pos ->Pos  x y)
pattern Size' w h <- (size->Size w h)

----------

class HasPos t
  where
    pos :: t -> Pos
    pos = liftA2 Pos _x _y

    _x :: t -> Int
    _x = _x . pos

    _y :: t -> Int
    _y = _y . pos

    mapP :: (Pos -> Pos) -> (t -> t)
    mapP = flip const

class HasSize t
  where
    size :: t -> Size
    size = liftA2 Size width height

    width :: t -> Int
    width (Size' w _) = w

    height :: t -> Int
    height (Size' _ h) = h


instance HasPos Pos
  where
    pos         = id
    mapP        = id
    _x (V2 x _) = x
    _y (V2 _ y) = y

instance HasSize Size
  where
    size            = id
    width  (V2 x _) = x
    height (V2 _ y) = y


instance HasPos Rect
  where
    pos    (Rect p _) = p
    mapP f (Rect p s) = Rect (f p) s

instance HasSize Rect
  where
    size (Rect _ s) = s

------------

_w = width
_h = height

_tl t = pos t
_tr t = pos t + Pos (_w t - 1) 0
_br t = pos t + Pos (_w t - 1) (_h t - 1)
_bl t = pos t + Pos 0          (_h t - 1)


{-# INLINE bounds #-}
bounds
  = liftA2 Rect pos size

{-# INLINE posInside #-}
posInside t p
  = inRect p $ bounds t

{-# INLINE recInside #-}
recInside r1 r2
  = posInside r2 (_tl r1)
 && posInside r2 (_br r1)

{-# INLINE inRect #-}
inRect p1 (Rect p2 (Size w h))
  | Pos x y <- p1 - p2
  = x >= 0 && x < w
 && y >= 0 && y < h

