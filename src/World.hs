module World where

import Geometry
import WorldMap

import FOV as F
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as B


data World = World
  { camera   :: Pos
  , player   :: Pos
  , screen   :: Size
  , viewDist :: Int
  , visible  :: S.Set Pos
  , level    :: WorldMap
  , buffer   :: B.ByteString
  }

newWorld lvl = World
  { camera   = Pos  0 0
  , player   = Pos  0 0
  , screen   = Size 0 0
  , visible  = mempty
  , viewDist = 10
  , level    = lvl
  , buffer   = mempty
  }


instance HasPos World
  where
    pos _ = V2 0 0

getCam
  = liftA2 Rect camera screen

camView
  = liftA2 view getCam level


moveCam p@(V2 x y) w
  | Nothing <- wmsize (level w)
  = if p == camera w then Nothing else Just p
  | x  <   0 = moveCam (V2 0  y) w
  | y  <   0 = moveCam (V2 x  0) w
  | x  >  mx = moveCam (V2 mx y) w
  | y  >  my = moveCam (V2 x my) w
  | p == camera w = Nothing
  | let           = Just p
  where
    Just (Size ww wh) = wmsize (level w)
    sw = min ww $ _w $ screen w
    sh = min wh $ _h $ screen w
    mx = ww - sw
    my = wh - sh


