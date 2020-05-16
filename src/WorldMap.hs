{-# LANGUAGE UndecidableInstances #-}
module WorldMap where

import qualified Sized as S
import Geometry
import Indexable
import Memory
import FOV as F
import qualified Cellular as Cell

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Set as Set

import qualified Foreign.Ptr      as F
import qualified Foreign.Storable as F
import qualified Foreign.C.String as F

import System.IO


type WmTiles
  = SomeIndexable Pos Char

data WorldMap = WorldMap
  { wmsize :: Maybe Size
  , tiles  :: WmTiles
  , memory :: Memory
  }

instance HasPos WorldMap
  where
    pos _ = zero

instance Indexable WorldMap Pos Char
  where
    index       = index       . tiles
    unsafeIndex = unsafeIndex . tiles


wmMemSize wm =
  case wmsize wm of
    Nothing -> chunkSize
    Just s  -> s

stride = width . wmMemSize

fromChunk c
  = WorldMap
      { wmsize = Just $ liftA2 Size S.width S.height c
      , tiles  = Ix c
      , memory = emptyMemory
      }

cellMap = do
  cell <- Cell.mk45
  pure WorldMap
    { wmsize = Nothing
    , tiles  = Ix cell
    , memory = emptyMemory
    }

isTileSolid
  = (== '#')

wmSolid w i
  = w ! i == '#'

vFov rad p@(V2 px py) v@(View b wm)
  = Set.map fromJust
  . Set.delete Nothing
  $ F.digital rad (\(F.B (x,y)) -> getTile $ V2 px py + V2 x y) isClear
  where
    getTile p@(V2 x y)
      | not (posInside b p) = Nothing
      | Just s <- wmsize wm
      , not (posInside (Rect zero s) p) = Nothing
      | let = pure p
    isClear i
      | let = maybe False (not . isTileSolid) $ index wm =<< i

vFov' rad p v@(View b wm)
  = Set.map (+p)
  $ F.digital rad (\(F.B (x,y)) -> V2 x y) isClear
  where
    isClear
      = not . isTileSolid . unsafeIndex lvl
    lvl
      = focus (tiles wm) p

-------------

data View
   = View Rect WorldMap

view r wm
  = View r wm

instance HasPos View
  where
    pos    (View r w) = pos w + pos r
    mapP f (View r w) =
      View (mapP f r) (mapP f w)

instance HasSize View
  where
    size (View r w) = size r




vTile (View (Rect o _) wm) p
  = wm ! (p + o)

viewPos v@(View (Rect o s) wm) i
  | (y,x) <- divMod i (stride wm)
  = V2 x y - o

vPos2Ix (View (Rect' _ _ w _) wm) (V2 x y)
  = x + y * stride wm

vClamp (View (Rect o _) wm) p
  = p - o


vMemPtr (View r w) f
  = memPtr r (memory w) f

{-# INLINE vTransfer #-}
vTransfer v@(View _ w) mptr p@(V2 x y) =
  case mptr of
    Nothing  -> memSetPos p (w ! p) (memory w)
    Just ptr -> F.pokeElemOff ptr (wmPointToIndex p) $
                  F.castCharToCChar (w ! p)

{-# INLINE vPutLineIx #-}
vPutLineIx v ptr o l = do
  void $ hPutBufNonBlocking stdout (F.plusPtr ptr $ o) l

{-# INLINE vPutLinePos #-}
vPutLinePos v@(View _ wm) mptr p@(V2 x y) l =
  case mptr of
    Just ptr -> vPutLineIx v ptr (wmPointToIndex p) l
    Nothing  ->
      forM_ [0..l-1] \o -> putChar =<<
        memGetPos (Pos x y + Pos o 0) (memory wm)

wmPointToIndex
  p@(V2 x y)
    = my * sw + mx
 where
   Size sw sh = chunkSize
   (cx, mx) = divMod x sw
   (cy, my) = divMod y sh


-------------

houses = ln S.=== ln S.=== ln S.=== ln
  where
    ln = house S.||| house S.||| house S.||| house

house :: S.Chunk 40 35
house = S.Chunk
  "........................................\
  \........................................\
  \...##############......##############...\
  \...#............#......#............#...\
  \...#............#......#............#...\
  \...#............#......#............#...\
  \...#............#......#............#...\
  \...######..######......######..######...\
  \........................................\
  \........................................\
  \........................................\
  \...######..######......######..######...\
  \...#............#......#............#...\
  \...#............#......#............#...\
  \...#............#......#............#...\
  \...#............#......#............#...\
  \...###..#########......##############...\
  \.....#..#...............................\
  \...###..#########......##############...\
  \...#............#......#............#...\
  \...#............#......#............#...\
  \...#............#......#............#...\
  \...#............#......#............#...\
  \...#########..###......######..######...\
  \...........#..#.........................\
  \...........#..#.........................\
  \...........#..#.........................\
  \...#########..###......######..######...\
  \...#............#......#............#...\
  \...#...................#............#...\
  \...#...................#............#...\
  \...#............#......#............#...\
  \...##############......##############...\
  \........................................\
  \........................................"


