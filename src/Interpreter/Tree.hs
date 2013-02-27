-- | Tree container.

{-# LANGUAGE ExistentialQuantification #-}

module Interpreter.Tree (Tree(Empty, Leaf, Parent, Branch, Sprout), OptiF,
                         Functor(..), Foldable(..),
                         branch, force, lmap) where

import Prelude hiding (foldl, foldr)
import Data.Foldable

type OptiF u v = (u -> v) -> u

data Tree v a = Empty
              | Leaf a
              | Parent a (Tree v a)
              | Branch (Tree v a) (Tree v a)
              | forall u. Sprout (OptiF u v) (u -> Tree v a)


instance Functor (Tree v) where
   fmap _ Empty           = Empty
   fmap f (Leaf x)        = Leaf (f x)
   fmap f (Parent x t)    = Parent (f x) (fmap f t)
   fmap f (Branch t1 t2)  = Branch (fmap f t1) (fmap f t2)
   fmap f (Sprout opti t) = Sprout opti (\x -> fmap f (t x))


instance Foldable (Tree v) where
   foldl _ z Empty          = z
   foldl f z (Leaf x)       = f z x
   foldl f z (Parent x t)   = foldl f (f z x) t
   foldl f z (Branch t1 t2) = foldl f (foldl f z t1) t2
   foldl _ _ (Sprout _ _)   = error "Tree.foldr: Sprout"

   foldr _ z Empty          = z
   foldr f z (Leaf x)       = f x z
   foldr f z (Parent x t)   = f x (foldr f z t)
   foldr f z (Branch t1 t2) = foldr f (foldr f z t2) t1
   foldr _ _ (Sprout _ _)   = error "Tree.foldr: Sprout"


branch :: Tree v a -> Tree v a -> Tree v a
branch Empty t     = t
branch t     Empty = t
branch t1    t2    = Branch t1 t2


force :: Ord v => (Tree v a -> v) -> Tree v a -> Tree v a
force _   t @ Empty       = t
force _   t @ (Leaf _)    = t
force val (Parent x t)    = Parent x (force val t)
force val (Branch t1 t2)  = Branch (force val t1) (force val t2)
force val (Sprout opti t) = force val (t (opti val'))
      where val' x = val (force val (t x))


lmap :: (a -> Tree v b) -> Tree v a -> Tree v b
lmap _ Empty           = Empty
lmap f (Leaf x)        = f x
lmap f (Branch t1 t2)  = Branch (lmap f t1) (lmap f t2)
lmap f (Sprout opti t) = Sprout opti (\x -> lmap f (t x))
lmap _ (Parent _ _)    = error "Tree.lmap: Parent"

