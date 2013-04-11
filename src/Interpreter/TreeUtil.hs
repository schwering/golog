{-# LANGUAGE GADTs #-}

module Interpreter.TreeUtil where

import Prelude hiding (foldr)
import Data.Foldable
import Interpreter.Tree

cutoff :: Int -> Tree a b c -> Tree a b c
cutoff 0 _                   = Empty
cutoff _ t @ Empty           = t
cutoff _ t @ (Leaf _)        = t
cutoff n (Parent x t)        = Parent x (cutoff (n-1) t)
cutoff n (Branch t1 t2)      = Branch (cutoff (n-1) t1) (cutoff (n-1) t2)
cutoff n (Sprout val opti t) = Sprout val opti (\x -> cutoff (n-1) (t x))


cutparents :: Tree a b c -> Tree a b c
cutparents t @ Empty           = t
cutparents t @ (Leaf _)        = t
cutparents (Parent _ t)        = Branch (cutparents t) Empty
cutparents (Branch t1 t2)      = Branch (cutparents t1) (cutparents t2)
cutparents (Sprout val opti t) = Sprout val opti (\x -> cutparents (t x))


toList :: Tree Grown b c -> [c]
toList = foldr (:) []

