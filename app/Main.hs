module Main where

import Geometry
import WorldMap
import World
import Engine


main :: IO ()
main = do
  cell <- cellMap


  void $ flip runEngineT engine (newWorld $ cell)
  -- void $ flip runEngineT engine (newWorld $ fromChunk house)
  -- void $ flip runEngineT engine (newWorld $ fromChunk houses)

    -- { player   = Pos 38 20
    { player   = Pos 0 0
    -- , viewDist = 14
    , viewDist = 7
    }

