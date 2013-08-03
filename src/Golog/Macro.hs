{-# OPTIONS_GHC -fno-warn-orphans #-}

module Golog.Macro
  (prim, primf, test,
   atomic, star, plus, ifThenElse, while,
   pick, withCtrl) where

import Golog.Interpreter

prim :: a -> Prog a
prim = PseudoAtom . Atom . Prim

primf :: (Sit a -> a) -> Prog a
primf = PseudoAtom . Atom . PrimF

test :: (Sit a -> Bool) -> Prog a
test = PseudoAtom . Atom . Test

atomic :: Prog a -> Prog a
atomic = PseudoAtom . Complex

star :: Prog a -> Prog a
star p = Nondet [Nil, p `Seq` star p]

plus :: Prog a -> Prog a
plus p = Nondet [p, p `Seq` plus p]

ifThenElse :: (Sit a -> Bool) -> Prog a -> Prog a -> Prog a
ifThenElse phi p1 p2 = Nondet [test phi `Seq` p1, test (not.phi) `Seq` p2]

while :: (Sit a -> Bool) -> Prog a -> Prog a
while phi p = star (test phi `Seq` p) `Seq` test (not.phi)

pick :: [b] -> (b -> Prog a) -> Prog a
pick xs p = Nondet (map p xs)

withCtrl :: (Sit a -> Bool) -> Prog a -> Prog a
withCtrl phi p' = h p' `Seq` t
   where t = test phi
         h (Seq p1 p2)                      = h p1 `Seq` h p2
         h (Nondet ps)                      = Nondet (map h ps)
         h (Conc p1 p2)                     = Conc (h p1) (h p2)
         h pa@(PseudoAtom (Atom (Prim _)))  = PseudoAtom (Complex (t `Seq` pa))
         h pa@(PseudoAtom (Atom (PrimF _))) = PseudoAtom (Complex (t `Seq` pa))
         h pa@(PseudoAtom (Atom (Test _)))  = pa
         h (PseudoAtom (Complex p))         = PseudoAtom (Complex (h p))
         h Nil                              = Nil

