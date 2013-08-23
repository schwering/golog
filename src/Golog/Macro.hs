module Golog.Macro
  (TestAction(..),
   prim, primf, test,
   atomic, star, plus, loop, opt,
   ifThen, ifThenElse, if_, then_, else_, while,
   pick, withCtrl) where

import Golog.Interpreter

class TestAction a where
   testAction :: (Sit a -> Bool) -> a

prim :: a -> Prog a
prim = PseudoAtom . Atom . Prim

primf :: (Sit a -> a) -> Prog a
primf = PseudoAtom . Atom . PrimF

test :: TestAction a => (Sit a -> Bool) -> Prog a
test = prim . testAction

atomic :: Prog a -> Prog a
atomic = PseudoAtom . Complex

star :: Prog a -> Prog a
star p = Nondet [Nil, p `Seq` star p]

plus :: Prog a -> Prog a
plus p = Nondet [p, p `Seq` plus p]

loop :: Prog a -> Prog a
loop p = p `Seq` loop p

opt :: Prog a -> Prog a
opt p = Nondet [Nil, p]

ifThenElse :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a -> Prog a
ifThenElse phi p1 p2 = Nondet [test phi `Seq` p1, test (not.phi) `Seq` p2]

ifThen :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a
ifThen phi p1 = if_ phi (then_ p1) (else_ Nil)

newtype IfBranch a = IfBranch (Prog a)
newtype ElseBranch a = ElseBranch (Prog a)

if_ :: TestAction a => (Sit a -> Bool) -> IfBranch a -> ElseBranch a -> Prog a
if_ phi (IfBranch p1) (ElseBranch p2) = ifThenElse phi p1 p2

then_ :: Prog a -> IfBranch a
then_ = IfBranch

else_ :: Prog a -> ElseBranch a
else_ = ElseBranch

while :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a
while phi p = star (test phi `Seq` p) `Seq` test (not.phi)

pick :: [b] -> (b -> Prog a) -> Prog a
pick xs p = Nondet (map p xs)

withCtrl :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a
withCtrl phi p' = h p' `Seq` t
   where t = test phi
         h (Seq p1 p2)                      = h p1 `Seq` h p2
         h (Nondet ps)                      = Nondet (map h ps)
         h (Conc p1 p2)                     = Conc (h p1) (h p2)
         h pa@(PseudoAtom (Atom (Prim _)))  = PseudoAtom (Complex (t `Seq` pa))
         h pa@(PseudoAtom (Atom (PrimF _))) = PseudoAtom (Complex (t `Seq` pa))
         h (PseudoAtom (Complex p))         = PseudoAtom (Complex (h p))
         h Nil                              = Nil

