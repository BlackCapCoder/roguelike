{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Sized where


import Geometry hiding (HasSize(..))
import Indexable

import Data.List (transpose)
import GHC.TypeLits (type (<=?))

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B



newtype Chunk (w :: Nat) (h :: Nat)
  = Chunk { unChunk :: B.ByteString }

-- instance KnownNat w => Show (Chunk w h) where
--   show = T.unpack . T.unlines . rows


width :: ∀ w h i. _ => Chunk w h -> i
width _ = fi . natVal $ Proxy @w

height :: ∀ w h i. _ => Chunk w h -> i
height _ = fi . natVal $ Proxy @h

bounds = liftA2 (,)       width height
size   = liftA2 (*)       width height
rows   = liftA2 chunksOfB width unChunk

chunksOfB n (B.splitAt n->(l,r))
  | B.null r = [l]
  | let      = l : chunksOfB n r

instance (KnownNat w, KnownNat h) => Indexable (Chunk w h) Pos Char
  where
    index c p@(V2 x y)
      | x < 0 || x >= width  c = Nothing
      | y < 0 || y >= height c = Nothing
      | otherwise = Just (unsafeIndex c p)

    unsafeIndex c@(Chunk txt) (V2 x y)
      = B.w2c $ B.unsafeIndex txt do y * width c + x

    {-# INLINE index #-}
    {-# INLINE unsafeIndex #-}

--------

below :: Chunk w h1 -> Chunk w h2 -> Chunk w (h1 + h2)
below (Chunk a) (Chunk b)
  = Chunk (a <> b)

beside :: _ => Chunk w1 h -> Chunk w2 h -> Chunk (w1 + w2) h
beside a b
  = Chunk $ mconcat . join $ transpose [rows a, rows b]


solid :: ∀ w h. _ => Char -> Chunk w h
solid chr = r
  where r = Chunk . B.pack $ replicate (size r) chr :: Chunk w h

blank :: ∀ w h. _ => Chunk w h
blank = solid ' '


padT :: ∀ p w h. _ => Chunk w h -> Chunk w (p + h)
padT = below do blank @_ @p

padL :: ∀ p w h. _ => Chunk w h -> Chunk (p + w) h
padL = beside do blank @p

padB :: ∀ p w h. _ => Chunk w h -> Chunk w (p + h)
padB = flip below do blank @_ @p

padR :: ∀ p w h. _ => Chunk w h -> Chunk (p + w) h
padR = flip beside do blank @p

-------------

type family If c t f where
  If True  t _ = t
  If False _ f = f

type Max l r
  = If (l <=? r) r l

beside' :: _ => Chunk w1 h1 -> Chunk w2 h2 -> Chunk (w1 + w2) (Max h1 h2)
beside' l r = padB l ||| padB r

below' :: _ => Chunk w1 h1 -> Chunk w2 h2 -> Chunk (Max w1 w2) (h1 + h2)
below' l r = padR l === padR r

infixr 3 |||; (|||) = beside
infixr 4 ===; (===) = below

infixr 3 |-|; (|-|) = beside'
infixr 4 =-=; (=-=) = below'

-------------

border :: ∀ p w h. _ => Char -> Chunk w h -> Chunk (p+p+w) (p+p+h)
border chr a
  = v === (h ||| a ||| h) === v
  where
    h = solid chr :: Chunk p h
    v = solid chr :: Chunk (w+p+p) p

box :: ∀ b w h. _ => _
box = border @b '#' $ blank @w @h

box' = box @1 @5 @5

