{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Interpreter.Tree2 where

import Data.Monoid

data Tree a where
   Empty :: Tree a
   Alt   :: [Tree a] -> Tree a
   Val   :: a -> Tree a -> Tree a

instance Monoid (Tree a) where
   mempty                = Empty
   mappend Empty      t2 = t2
   mappend (Alt ts)   t2 = Alt (fmap (\t1 -> mappend t1 t2) ts)
   mappend (Val x t1) t2 = Val x (mappend t1 t2)

instance Functor Tree where
   fmap _ Empty     = Empty
   fmap f (Alt ts)  = Alt (map (fmap f) ts)
   fmap f (Val x t) = Val (f x) (fmap f t)

itl :: Tree a -> Tree a -> Tree a
itl Empty          t2             = t2
itl t1             Empty          = t1
itl (Alt ts)       t2             = Alt (map (\t1 -> itl t1 t2) ts)
itl t1             (Alt ts)       = Alt (map (\t2 -> itl t1 t2) ts)
itl t1@(Val x1 r1) t2@(Val x2 r2) = Alt [Val x1 (itl t2 r1), Val x2 (itl r2 t1)]

toLists :: Tree a -> [[a]]
toLists Empty     = [[]]
toLists (Alt ts)  = concat (map toLists ts)
toLists (Val x t) = [x:xs | xs <- toLists t]

