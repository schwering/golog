{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Binary trees whose elements can be stored in leaves or inner nodes.
-- Trees can be fully grown or not. Ungrown trees can contain sprouts which
-- are represent tree functions of some element; this element is picked
-- according to an optimization and evaluation function by 'force'.

module Golog.Old.Tree (Tree(..), Depth, ValF, OptiF, Grown, Ungrown,
                       Functor(..), Foldable(..),
                       branch, force, lmap, best) where

import Prelude hiding (foldl, foldr, max)
import Data.Foldable

type Depth = Int
type ValF a v = Tree Grown a a -> v
type OptiF u v = (u -> v) -> u

data Grown
data Ungrown

data Tree a b c where
   Empty  :: Tree a b c
   Leaf   :: c -> Tree a b c
   Parent :: c -> Tree a b c -> Tree a b c
   Branch :: Tree a b c -> Tree a b c -> Tree a b c
   Sprout :: Ord v => ValF b v -> OptiF u v -> (u -> Tree Ungrown b c) -> Tree Ungrown b c


instance Functor (Tree a b) where
   fmap _ Empty               = Empty
   fmap f (Leaf x)            = Leaf (f x)
   fmap f (Parent x t)        = Parent (f x) (fmap f t)
   fmap f (Branch t1 t2)      = Branch (fmap f t1) (fmap f t2)
   fmap f (Sprout val opti t) = Sprout val opti (\x -> fmap f (t x))


instance Foldable (Tree Grown b) where
   foldl _ z Empty          = z
   foldl f z (Leaf x)       = f z x
   foldl f z (Parent x t)   = foldl f (f z x) t
   foldl f z (Branch t1 t2) = foldl f (foldl f z t1) t2

   foldr _ z Empty          = z
   foldr f z (Leaf x)       = f x z
   foldr f z (Parent x t)   = f x (foldr f z t)
   foldr f z (Branch t1 t2) = foldr f (foldr f z t2) t1


branch :: Tree a b c -> Tree a b c -> Tree a b c
branch Empty Empty = Empty
branch Empty t     = t
branch t     Empty = t
branch t1    t2    = Branch t1 t2


force :: Tree a b b -> Tree Grown b b
force Empty               = Empty
force (Leaf x)            = Leaf x
force (Parent x t)        = Parent x (force t)
force (Branch t1 t2)      = branch (force t1) (force t2)
force (Sprout val opti t) = force (t (opti (val . force . t)))


lmap :: (c -> Tree a b d) -> Tree a b c -> Tree a b d
lmap _ Empty               = Empty
lmap f (Leaf x)            = f x
lmap f (Branch t1 t2)      = branch (lmap f t1) (lmap f t2)
lmap f (Sprout val opti t) = Sprout val opti (\x -> lmap f (t x))
lmap _ (Parent _ _)        = error "Tree.lmap: Parent"


best :: c -> (c -> c -> c) -> (c -> Bool) -> Depth -> Tree Grown b c -> c
best def _   _   _ Empty                    = def
best def max cut l (Parent x t) | l == 0    = x
                                | cut x     = max x (best def max cut (l-1) t)
                                | l > 0     = best def max cut (l-1) t
                                | otherwise = error "Tree.best: l < 0"
best def max cut l (Branch t1 t2)           = max (best def max cut l t1)
                                                  (best def max cut l t2)
best _   _   _   _ (Leaf x)                 = x

