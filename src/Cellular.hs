module Cellular where

import Zipper
import Data.Stream.Infinite as S
import Prelude as P

import Control.Comonad
import System.Random


horiz = [left, right]
vert  = [up, down]
cardi = horiz <> vert
diag  = liftM2 (.) horiz vert

neighbours =
  cardi <> diag


ruleGOL :: Z Bool -> Bool
ruleGOL z =
  case aliveNeighbours z of
    2 -> extract z
    3 -> True
    _ -> False

rule45 :: Z Bool -> Bool
rule45 z =
  case aliveNeighbours z of
    0 -> True
    4 -> extract z
    n -> n >= 5

rule45' :: Z Bool -> Bool
rule45' z =
  case aliveNeighbours z of
    0 -> True
    4 -> extract z
    n -> n >= 5

ruleClean :: Z Bool -> Bool
ruleClean z =
  case aliveNeighbours z of
    0 -> False
    8 -> False
    _ -> extract z

ruleSmallClean :: Z Bool -> Bool
ruleSmallClean z =
  case aliveNeighbours z of
    0 -> False
    -- 1 -> False
    _ -> extract z

ruleEven :: Z Bool -> Bool
ruleEven =
  even . aliveNeighbours

ruleDiag z
  = aliveDiags z > aliveCardis z

ruleCardi z
  = aliveDiags z < aliveCardis z

ruleExtreme :: Z Bool -> Bool
ruleExtreme z =
  case aliveNeighbours z of
    0 -> True
    1 -> True
    2 -> True
    8 -> True
    9 -> True
    _ -> False


alives dirs z
  = card $ map (\dir -> extract $ dir z) dirs

aliveNeighbours :: Z Bool -> Int
aliveNeighbours =
  alives neighbours

aliveDiags
  = alives diag

aliveCardis
  = alives cardi

card :: [Bool] -> Int
card = length . P.filter (==True)



------

randomS :: (RandomGen g, Random a) => g -> Stream a
randomS (random->(a, g))
  = a :> randomS g

randomCells :: (RandomGen g) => Float -> g -> Stream Bool
randomCells prob g =
  (< prob) <$> randomS g

gens (split->(g,gs))
  = g :> gens gs

rngZ (split->((gens->gl),(gens->gr)))
  = Z $ LZ' (line gl) (line gr)
  where
    cells45 =
      randomCells 0.45

    line (g:>gs:>gss) =
      LZ' (cells45 g) (cells45 gs) :> line gss

evolve n f
  = (S.!! n) . S.iterate (extend f)

mk45 = do
  lvl <- rngZ <$> newStdGen
  pure . fmap toChar

       . evolve  2 ruleSmallClean
       . evolve  4 ruleExtreme
       . evolve  4 ruleSmallClean
       . evolve  2 rule45'
       . evolve 10 ruleClean
       . evolve  1 ruleCardi
       . evolve  6 rule45'
       . evolve  1 ruleGOL
       . evolve  2 rule45'
       . fmap not
       . evolve 10 rule45
       $ lvl

toChar = \ case
  True  -> '#'
  False -> '.'


