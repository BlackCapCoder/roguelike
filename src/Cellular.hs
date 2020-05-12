module Cellular where

import Zipper
import Stream as S

import Control.Comonad
import System.Random


neighbours :: [Z a -> Z a]
neighbours =
  horiz ++ vert ++ liftM2 (.) horiz vert
    where
      horiz = [left, right]
      vert  = [up, down]

aliveNeighbours :: Z Bool -> Int
aliveNeighbours z =
  card $ map (\dir -> extract $ dir z) neighbours

card :: [Bool] -> Int
card = length . filter (==True)


ruleGOL :: Z Bool -> Bool
ruleGOL z =
  case aliveNeighbours z of
    2 -> extract z
    3 -> True
    _ -> False

rule45 :: Z Bool -> Bool
rule45 z =
  case aliveNeighbours z of
    -- 0 -> True
    4 -> extract z
    n -> n >= 5

------

randomS :: (RandomGen g, Random a) => g -> Stream a
randomS (random->(a, g))
  = a :- randomS g

randomCells :: (RandomGen g) => Float -> g -> Stream Bool
randomCells prob g =
  (< prob) <$> randomS g

gens (split->(g,gs))
  = g :- gens gs

rngZ (split->((gens->gl),(gens->gr)))
  = Z $ LZ' (line gl) (line gr)
  where
    cells45 =
      randomCells 0.45

    line (g:-gs:-gss) =
      LZ' (cells45 g) (cells45 gs) :- line gss

mk45 n =
  index n . S.iterate (extend rule45) . rngZ <$> newStdGen

mk45' n = map f <$> mk45 n
  where
    f False = '.'
    f True  = '#'
