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


cellMap gens = do
  cell <- Cell.mk45' gens
  pure WorldMap
    { wmsize = Nothing
    , tiles  = Ix cell
    , memory = emptyMemory
    }

boringMap
  = WorldMap
    { wmsize = Nothing
    , tiles  = Ix Boring
    , memory = emptyMemory
    }

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

isTileSolid
  = (== '#')

wmSolid w i
  = w ! i == '#'

vFov rad p@(V2 px py) v@(View b wm)
  = Set.map fj
  . Set.filter isJust
  $ F.digital rad (\(F.B (x,y)) -> getTile $ V2 px py + V2 x y) isClear
  where
    getTile p@(V2 x y)
      | not (posInside b p) = Nothing
      | Just s <- wmsize wm
      , not (posInside (Rect zero s) p) = Nothing
      | let = pure p
    isClear i
      | let = maybe False (not . isTileSolid) $ index wm =<< i

    fj (Just x) = x
    fj Nothing  = error "What."

-- vFov rad p@(Pos px py) v@(View b wm)
--   = Set.delete (-1)
--   $ F.digital rad (\(F.B (x,y)) -> getTile $ Pos px py + Pos x y) isClear
--   where
--     getTile p@(Pos x y)
--       | not (posInside b p) = -1
--       | let = x + y * stride wm
--     isClear i
--       | i == -1 = False
--       | let = maybe False (not . isTileSolid) $ index wm i

-------------

data View
   = View Rect WorldMap

view r wm
  = View r wm
  -- | recInside r (bounds wm) = View r wm
  -- | otherwise               = error "BAD VIEW"

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


-- viewIx v@(View (Rect o s) wm) p
--   = ix wm (p+o)

viewPos v@(View (Rect o s) wm) i
  | (y,x) <- divMod i (stride wm)
  = V2 x y - o

vPos2Ix (View (Rect' _ _ w _) wm) (V2 x y)
  = x + y * stride wm

vClamp (View (Rect o _) wm) p
  = p - o


vMemPtr (View r w) f
  = memPtr r (memory w) f
  -- = f ()

{-# INLINE vTransfer #-}
vTransfer v@(View _ w) mptr p@(V2 x y) =
  case mptr of
    Nothing  -> memSetPos p (w ! p) (memory w)
    Just ptr -> F.pokeElemOff ptr i $ F.castCharToCChar (w ! p)
 where
   Size sw sh = chunkSize
   (cx, mx) = divMod x sw
   (cy, my) = divMod y sh
   i        = my * sw + mx

{-# INLINE vPutLineIx #-}
vPutLineIx v ptr o l = do
  hPutBuf stdout (F.plusPtr ptr $ o) l

{-# INLINE vPutLinePos #-}
vPutLinePos v@(View _ wm) mptr p@(V2 x y) l =
  case mptr of
    Just ptr -> vPutLineIx v ptr i l
    Nothing  ->
      forM_ [0..l-1] \o -> putChar =<<
        memGetPos (Pos x y + Pos o 0) (memory wm)
 where
   Size sw sh = chunkSize
   (cx, mx) = divMod x sw
   (cy, my) = divMod y sh
   i        = my * sw + mx


memAll wm
  | Just (Size w h) <- wmsize wm = do
    forM_ [0..h-1] \y ->
      forM_ [0..w-1] \x ->
        memSetPos (V2 x y) (tiles wm ! V2 x y) (memory wm)

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


