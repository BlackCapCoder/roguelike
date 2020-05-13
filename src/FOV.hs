module FOV where

import Data.List (foldl1', find, groupBy)
import qualified Data.Set as S


newtype Bump = B (Int, Int)
  deriving (Show, Eq)

type Distance = Int
type Progress = Int

type Line         = (Bump, Bump)
type ConvexHull   = [Bump]
type Edge         = (Line, ConvexHull)
type EdgeInterval = (Edge, Edge)


type Level a = a -> Bool


pattern Line a b = (a, b) :: Line
pattern Edge a b = (a, b) :: Edge
pattern Intv a b = (a, b) :: EdgeInterval
pattern Bump a b = B (a, b)

pattern Line' x1 y1 x2 y2
  = (Bump x1 y1, Bump x2 y2) :: Line


steeper :: Bump -> Bump -> Bump -> Bool
steeper (B(xf, yf)) (B(x1, y1)) (B(x2, y2)) =
  (yf - y1)*(xf - x2) >= (yf - y2)*(xf - x1)

addHull :: (Bump -> Bump -> Bool) -> Bump -> ConvexHull -> ConvexHull
addHull gte d l =
  case l of
    a:b:cs ->
      if gte a b
      then addHull gte d (b:cs)
      else d : l
    _ -> d : l


intersect :: Line -> Distance -> (Int, Int)
intersect (B(x, y), B(xf, yf)) d =
  ((d - y)*(xf - x) + x*(yf - y), yf - y)


divUp n k = ((n + k - 1) `div` k) - 1


-- scan :: ∀ a. Ord a => Distance -> (Bump -> a) -> Level a -> S.Set a
scan r tr isClear
  = dscan 1
  $ Intv do Edge (Line' 1 0 ( -r) r) [B(0, 0)]
         do Edge (Line' 0 0 (r+1) r) [B(1, 0)]

 where

  -- dscan :: Distance -> EdgeInterval -> S.Set a
  dscan d (s0@(Edge sl sBumps0), e@(Edge el eBumps0))
    | d > r
    = mempty

    | (n, k) <- intersect sl d
    , ps0    <- div n k
    = S.union do S.fromDistinctAscList $ tr' <$> [ps0..pe]
    . f ps0 s0
    . map head . drop 1
    $ groupBy (on (==) $ isClear . tr') [ps0..pe]

   where

    f p a ps
      | isClear $ tr' p = f' a ps --fC a ps
      | p:pss <- ps     = f' (edge p not eBumps0 sBumps0) pss
      | let             = mempty

    f' acc = dig acc . split

    split
      = foldr (\x (l,r) -> (r, x:l)) ([],[])

    dig acc (pp:ls, p:rs)
      | a <- edge  p  id (snd acc) eBumps0
      , b <- edge pp not  eBumps0  sBumps0
      = uscan acc a <> dig b (ls,rs)
    dig acc ([],[p])
      | a <- edge  p  id (snd acc) eBumps0
      = uscan acc a
    dig acc ([],[])
      = uscan acc e

    uscan acc x
      = dscan (d+1) (Intv acc x)


    pe | (n, k) <- intersect el d
       = divUp n k

    dbump x
      = Bump x d

    tr'
      = tr . dbump

    edge ps a b
      = Edge (Line (nsp b) bump) . addHull gte bump
      where
        bump    = dbump ps
        gte x y = a $ steeper bump x y
        nsp     = foldl1' \acc e -> if gte e acc then e else acc



-- digital :: Ord a => Distance -> (Bump -> a) -> Level a -> S.Set a
digital r trL lvl
  = S.unions $ map (\tr -> scan r tr lvl) [qtr0, qtr1, qtr2, qtr3]
  where
    qtr0 (B(x, y)) = trL $ B (  x, - y)
    qtr1 (B(x, y)) = trL $ B (  y,   x)
    qtr2 (B(x, y)) = trL $ B (- x,   y)
    qtr3 (B(x, y)) = trL $ B (- y, - x)

