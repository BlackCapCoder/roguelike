module BTree where

import Control.Comonad


data BTree a
    = Leaf a
    | Branch a (BTree a) (BTree a)
    deriving
      ( Show, Eq, Ord
      , Functor, Foldable, Traversable
      , ComonadApply
      )

instance Comonad BTree
  where
    extract = \case
      Leaf   a     -> a
      Branch a _ _ -> a

    duplicate x = case x of
      Leaf   _     -> Leaf   x
      Branch _ l r -> Branch x (duplicate l) (duplicate r)

    extend f x = case x of
      Leaf   _     -> Leaf   (f x)
      Branch _ l r -> Branch (f x) (extend f l) (extend f r)


instance Applicative BTree
  where
    pure = Leaf

    Leaf f <*> Leaf a = Leaf (f a)

    -- zipping:
    Leaf f <*> Branch x _ _ = Leaf (f x)
    Branch f _ _ <*> Leaf x = Leaf (f x)

    Branch f fl fr <*> Branch x xl xr
      = Branch (f x) (fl <*> xl) (fr <*> xr)

