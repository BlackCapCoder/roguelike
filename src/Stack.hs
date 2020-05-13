module Stack where

import Control.Comonad
import Prelude hiding (tail)
import qualified Data.List as L
import qualified Data.Stream.Infinite as S
import qualified Data.Stream.Infinite.Skew as SK


class IsStack t
  where
    pop :: t a -> (a, t a)

    safePop :: t a -> Maybe (a, t a)

    tail :: t a -> t a
    tail =
      snd . pop

    safeTail :: t a -> Maybe (t a)
    safeTail =
      fmap snd . safePop

    push :: a -> t a -> t a

    default push :: (Applicative t, Semigroup (t a)) => a -> t a -> t a
    push x xs = pure x <> xs

    default pop :: Foldable t => t a -> (a, t a)
    pop = liftA2 (,) head tail

    default safePop :: Foldable t => t a -> Maybe (a, t a)
    safePop t =
      liftA2 (,) (safeHead t) (safeTail t)


infixr  5 :-, :~
pattern x :- xs <- (pop->(x, xs))
  where x :- xs  = push x xs

pattern x :~ xs <- (safePop->Just(x, xs))
  where x :~ xs  = push x xs


-------


instance IsStack []
  where
    push    = (:)
    safePop = L.uncons
    tail    = L.tail

instance IsStack S.Stream
  where
    push    = (S.:>)
    tail    = S.tail
    safePop = Just . pop

instance IsStack SK.Stream
  where
    push    = (SK.<|)
    tail    = SK.tail
    pop     = SK.uncons
    safePop = Just . pop

