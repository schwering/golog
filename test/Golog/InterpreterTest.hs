{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE TemplateHaskell #-}

module Golog.InterpreterTest where

import Golog.Interpreter
import Test.QuickCheck.All

data Prim = A | B | C | D deriving (Eq, Show)

instance BAT Prim where
   data Sit Prim = S0 | Do Prim (Sit Prim)

   poss D _ = False
   poss _ _ = True

instance DTBAT Prim where
   reward A s = case s of Do A _ -> 0
                          _      -> 0
   reward B s = case s of Do B _ -> 0
                          _      -> 1
   reward C s = case s of Do C _ -> 0
                          _      -> 2
   reward D s = case s of Do D _ -> 0
                          _      -> 3

instance (Eq a) => Eq (Atom a) where
   Prim x == Prim y = x == y
   _      == Prim _ = False
   Prim _ == _      = False
   _      == _      = error "GologTest.Eq.==: cannot compare non-Prims"

instance (Eq a) => Eq (PseudoAtom a) where
   Atom    x == Atom y    = x == y
   Complex x == Complex y = x == y
   _         == _         = False

instance (Eq a) => Eq (Prog a) where
   Seq u v      == Seq x y      = u == x && v == y
   Nondet u     == Nondet x     = u == x 
   Conc u v     == Conc x y     = u == x && v == y
   PseudoAtom x == PseudoAtom y = x == y
   Nil          == Nil          = True
   _            ==   _          = False



prop_three :: a -> Bool
prop_three = const True


runTests :: IO Bool
runTests = $quickCheckAll


