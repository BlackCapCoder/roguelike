module Stream where

import Indexable

import qualified Data.List as L
import Prelude hiding (take, iterate, tail, repeat)


data Stream a = a :- Stream a
  deriving
    ( Functor, Foldable, Traversable
    , Eq, Ord
    )

infixr 5 :-

instance Applicative Stream
  where
    pure
      = repeat

    f :- fs <*> a :- as
      = f a :- do fs <*> as

instance Indexable (Stream a) Int a
  where
    unsafeIndex (x :- _) 0 = x
    unsafeIndex (_ :- x) n = unsafeIndex x (n - 1)

------

tail (_ :- xs) = xs

take n = L.take n . toList

iterate f x = x :- iterate f (f x)

repeat a = a :- repeat a

cons init rest
   = foldr (:-) rest init

fromList
  = flip cons bottom

bottom :: Stream a
bottom = bottom

index i = (!! i) . toList

