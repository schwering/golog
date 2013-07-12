{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-} 

module Interpreter.Golog3 where

import Control.Monad.State.Lazy
import Data.Monoid

class BAT a where
   data Sit a :: *
   s0         :: Sit a
   do_        :: a -> Sit a -> Sit a
   poss       :: a -> Sit a -> Bool

data Atom a = Prim a
            | PrimF (Sit a -> a)
            | Test (Sit a -> Bool)

data Prog a where
   Seq    :: Prog a -> Prog a -> Prog a
   Nondet :: Prog a -> Prog a -> Prog a
   Star   :: Prog a -> Prog a
   Pick   :: [b] -> (b -> Prog a) -> Prog a
   Atom   :: Atom a -> Prog a
   Nil    :: Prog a

val :: BAT a => Prog a -> Sit a -> [Sit a]
val (Seq p1 p2)    s = concat $ map (val p2) (val p1 s)
val (Nondet p1 p2) s = val p1 s ++ val p2 s
val (Star p)       s = concat $ map (\p' -> val p' s) (iterate (Seq p) Nil)
val (Pick dom p)   s = concat $ map (\x -> val (p x) s) dom
val (Atom a)       s = val' a s
val Nil            s = [s]

val' :: BAT a => Atom a -> Sit a -> [Sit a]
val' (Prim a)  s | poss a s     = [do_ a s]
                 | otherwise    = []
val' (PrimF a) s | poss (a s) s = [do_ (a s) s]
                 | otherwise    = []
val' (Test t)  s | t s          = [s]
                 | otherwise    = []

instance BAT Char where
   data Sit Char = Do Char (Sit Char) | S0 deriving Show
   s0 = S0
   do_ = Do
   poss y (Do x s) = x <= y
   poss _ S0       = True

p = Atom . Prim
t = Atom . Test

