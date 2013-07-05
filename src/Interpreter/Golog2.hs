{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE Rank2Types #-}

module Interpreter.Golog2 where

import Prelude
import Data.Monoid

type Reward = Double

class BAT a where
   data Sit a :: *
   s0         :: Sit a
   do_        :: a -> Sit a -> Sit a
   poss       :: a -> Sit a -> Bool
   reward     :: a -> Sit a -> Reward


data Atom a = Prim a
            | PrimF (Sit a -> a)
            | Test (Sit a -> Bool)

data Prog a where
   Seq    :: Prog a -> Prog a -> Prog a
   Nondet :: Prog a -> Prog a -> Prog a
   Conc   :: Prog a -> Prog a -> Prog a
   Star   :: Prog a -> Prog a
   Atom   :: Atom a -> Prog a
   Nil    :: Prog a


data Tree a where
   Alternative :: [Tree a] -> Tree a
   Value       :: a -> Tree a -> Tree a
   Empty       :: Tree a

instance Monoid (Tree a) where
   mempty                      = Empty

   mappend (Alternative ts) t2 = Alternative (fmap (\t1 -> mappend t1 t2) ts)
   mappend (Value x t1) t2     = Value x (mappend t1 t2)
   mappend Empty t2            = t2
   mappend t1    Empty         = t1
   
instance Functor Tree where
   fmap f (Alternative ts) = Alternative (map (fmap f) ts)
   fmap f (Value x t)      = Value (f x) (fmap f t)
   fmap _ Empty            = Empty


den :: Prog a -> Tree (Atom a)
den (Seq p1 p2)    = mappend (den p1) (den p2)
den (Nondet p1 p2) = Alternative [den p1, den p2]
den (Conc p1 p2)   = undefined
den (Star p)       = Alternative (map den (iterate (Seq p) Nil))
den (Atom a)       = Value a Empty
den Nil            = Empty

