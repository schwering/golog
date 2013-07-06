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

data PseudoAtom a = Atom (Atom a)
                  | Complex (Prog a)

data Prog a where
   Seq        :: Prog a -> Prog a -> Prog a
   Nondet     :: Prog a -> Prog a -> Prog a
   Conc       :: Prog a -> Prog a -> Prog a
   Star       :: Prog a -> Prog a
   Pick       :: [b] -> (b -> Prog a) -> Prog a
   PseudoAtom :: PseudoAtom a -> Prog a
   Nil        :: Prog a

data Tree a where
   Alt   :: [Tree a] -> Tree a
   Val   :: a -> Tree a -> Tree a
   Empty :: Tree a
   deriving Show

instance Monoid (Tree a) where
   mempty                = Empty

   mappend (Alt ts)   t2 = Alt (fmap (\t1 -> mappend t1 t2) ts)
   mappend (Val x t1) t2 = Val x (mappend t1 t2)
   mappend Empty      t2 = t2
   
instance Functor Tree where
   fmap f (Alt ts)  = Alt (map (fmap f) ts)
   fmap f (Val x t) = Val (f x) (fmap f t)
   fmap _ Empty     = Empty

itl :: Tree a -> Tree a -> Tree a
itl (Alt ts)        t2              = Alt (map (\t1 -> itl t1 t2) ts)
itl t1              (Alt ts)        = Alt (map (\t2 -> itl t1 t2) ts)
itl Empty           t2              = t2
itl t1              Empty           = t1
itl t1@(Val x1 t1') t2@(Val x2 t2') = Alt [Val x1 (itl t2 t1'),
                                           Val x2 (itl t2' t1)]

toLists :: Tree a -> [[a]]
toLists (Alt ts)  = concat (map toLists ts)
toLists (Val x t) = [x:xs | xs <- toLists t]
toLists Empty     = [[]]

den' :: Prog a -> Tree (PseudoAtom a)
den' (Seq p1 p2)    = mappend (den' p1) (den' p2)
den' (Nondet p1 p2) = Alt [den' p1, den' p2]
den' (Conc p1 p2)   = itl (den' p1) (den' p2)
den' (Star p)       = Alt (map den' (iterate (Seq p) Nil))
den' (Pick dom p)   = Alt (map den' (map p dom))
den' (PseudoAtom a) = Val a Empty
den' Nil            = Empty

den :: Prog a -> Tree (Atom a)
den p' = rec (den' p')
   where rec :: Tree (PseudoAtom a) -> Tree (Atom a)
         rec (Alt ts)            = Alt (map rec ts)
         rec (Val (Complex p) t) = mappend (den p) (rec t)
         rec (Val (Atom a)    t) = Val a (rec t)
         rec Empty               = Empty

