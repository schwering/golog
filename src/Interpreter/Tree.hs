-- | Tree container.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Interpreter.Tree (Tree(..), Depth, ValF, OptiF,
                         Functor(..), Foldable(..),
                         branch, force, lmap, best) where

import Prelude hiding (foldl, foldr, max)
import Data.Foldable

type Depth = Int
type ValF a v = Tree a a -> v
type OptiF u v = (u -> v) -> u

data Tree a b where
   Empty  :: Tree a b
   Leaf   :: b -> Tree a b
   Parent :: b -> Tree a b -> Tree a b
   Branch :: Tree a b -> Tree a b -> Tree a b
   Sprout :: Ord v => ValF a v -> OptiF u v -> (u -> Tree a b) -> Tree a b


instance Functor (Tree a) where
   fmap _ Empty               = Empty
   fmap f (Leaf x)            = Leaf (f x)
   fmap f (Parent x t)        = Parent (f x) (fmap f t)
   fmap f (Branch t1 t2)      = Branch (fmap f t1) (fmap f t2)
   fmap f (Sprout val opti t) = Sprout val opti (\x -> fmap f (t x))


instance Foldable (Tree a) where
   foldl _ z Empty          = z
   foldl f z (Leaf x)       = f z x
   foldl f z (Parent x t)   = foldl f (f z x) t
   foldl f z (Branch t1 t2) = foldl f (foldl f z t1) t2
   foldl _ _ (Sprout _ _ _) = error "Tree.foldr: Sprout"

   foldr _ z Empty          = z
   foldr f z (Leaf x)       = f x z
   foldr f z (Parent x t)   = f x (foldr f z t)
   foldr f z (Branch t1 t2) = foldr f (foldr f z t2) t1
   foldr _ _ (Sprout _ _ _) = error "Tree.foldr: Sprout"


branch :: Tree a b -> Tree a b -> Tree a b
branch Empty t     = t
branch t     Empty = t
branch t1    t2    = Branch t1 t2


force :: Tree a a -> Tree a a
force t @ Empty           = t
force t @ (Leaf _)        = t
force (Parent x t)        = Parent x (force t)
force (Branch t1 t2)      = Branch (force t1) (force t2)
force (Sprout val opti t) = force (t (opti (val . force . t)))


lmap :: (b -> Tree a c) -> Tree a b -> Tree a c
lmap _ Empty               = Empty
lmap f (Leaf x)            = f x
lmap f (Branch t1 t2)      = Branch (lmap f t1) (lmap f t2)
lmap f (Sprout val opti t) = Sprout val opti (\x -> lmap f (t x))
lmap _ (Parent _ _)        = error "Tree.lmap: Parent"


best :: b -> (b -> b -> b) -> (b -> Bool) -> Depth -> Tree a b -> b
best def _   _   _ Empty                    = def
best def max cut l (Parent x t) | l == 0    = x
                                | cut x     = max x (best def max cut (l-1) t)
                                | l > 0     = best def max cut (l-1) t
                                | otherwise = error "Tree.best: l < 0"
best def max cut l (Branch t1 t2)           = max (best def max cut l t1)
                                                  (best def max cut l t2)
best _   _   _   _ (Leaf x)                 = x
best _   _   _   _ (Sprout _ _ _)           = error "Tree.best: Sprout"

