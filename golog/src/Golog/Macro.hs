module Golog.Macro
  (TestAction(..),
   prim, primf, test,
   atomic, choice, star, plus, loop, opt,
   ifThen, ifThenElse, if_, then_, else_, while, when, unless,
   ifA, whenA, unlessA, ifThenElseA, whileA, until, untilA,
   forSome, forAll, pick, withCtrl, monitor) where

import Prelude hiding (until)
import Data.List (foldl', foldl1')
import Golog.Interpreter

class TestAction a where
   testAction :: (Sit a -> Bool) -> a
   noop :: a
   noop = testAction (const True)

prim :: a -> Prog a
prim = Atom . Act . const

primf :: (Sit a -> a) -> Prog a
primf = Atom . Act

test :: TestAction a => (Sit a -> Bool) -> Prog a
test = prim . testAction

atomic :: Prog a -> Prog a
atomic = Atom . Complex

choice :: [Prog a] -> Prog a
choice []     = error "Golog.Macro.choice: empty list"
choice [p]    = p
choice (p:ps) = Choice p (choice ps)

star :: Prog a -> Prog a
star p = Choice Nil (p `Seq` star p)

plus :: Prog a -> Prog a
plus p = Choice p (p `Seq` plus p)

loop :: Prog a -> Prog a
loop p = p `Seq` loop p

opt :: Prog a -> Prog a
opt = Choice Nil

ifThenElse :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a -> Prog a
ifThenElse phi p1 p2 = Choice (test phi `Seq` p1) (test (not.phi) `Seq` p2)

ifThen :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a
ifThen phi p1 = if_ phi (then_ p1) (else_ Nil)

when :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a
when phi p = if_ phi (then_ p) (else_ Nil)

unless :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a
unless phi p = if_ phi (then_ Nil) (else_ p)

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

until :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a
until phi = while (not.phi)

forAll :: [b] -> (b -> Prog a) -> Prog a
forAll xs pf = foldl' (\p x -> p `Seq` pf x) Nil xs

forSome :: [b] -> (b -> Prog a) -> Prog a
forSome = pick

pick :: [b] -> (b -> Prog a) -> Prog a
pick []     _ = error "Golog.Macro.pick: empty list"
pick [x]    p = p x
pick (x:xs) p = Choice (p x) (pick xs p)


-- | If-then-else construct where the condition and the first atom of the branch
-- are atomic. This prevents concurrent programs from getting in between of them
-- (and making flipping the condition just evaluated).
ifThenElseA :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a -> Prog a
ifThenElseA phi p1 p2 = Choice (mergeAtomic (test phi) p1)
                               (mergeAtomic (test (not.phi)) p2)

mergeAtomic :: Prog a -> Prog a -> Prog a
mergeAtomic addon p = case nextPA p of
   [] -> atomic addon
   ds -> choice [atomic (addon `Seq` Atom c) `Seq` p' | (c, p') <- ds]

whenA :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a
whenA phi p = ifA phi (then_ p) (else_ Nil)

unlessA :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a
unlessA phi p = ifA phi (then_ Nil) (else_ p)

ifA :: TestAction a => (Sit a -> Bool) -> IfBranch a -> ElseBranch a -> Prog a
ifA phi (IfBranch p1) (ElseBranch p2) = ifThenElseA phi p1 p2

whileA :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a
whileA phi p = star (mergeAtomic (test phi) p) `Seq` test (not.phi)

untilA :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a
untilA phi = whileA (not.phi)


-- | Asserts that the test holds before every action of the first program.
--
-- Note: Atomic actions in the program are not split up, i.e., during the
-- execution of complex atomic actions we don't assert the test!
--
-- Note: The test action and the subsequent atom may be ripped apart if there's
-- another program running concurrently. Making them an atomic complex action
-- would be easy, but only at the cost of moving the test action inside the
-- 'Choice' branches, which to avoid is the idea of 'intersperse'.
--
-- This construct is taken from ReadyLog:
--
-- A. Ferrein and G. Lakemeyer. Logic-based robot control in highly dynamic
-- domains. Robotics and Autonomous Systems, 56(11):980-991, 2008.
withCtrl :: TestAction a => (Sit a -> Bool) -> Prog a -> Prog a
withCtrl phi = intersperse (test phi)

-- | Creates a large reactive program. If the list of programs is @[p1, ...,
-- pn]@, the (i-1)th program is executed entirely before every action of the ith
-- program.
--
-- Usually the higher-precendence programs should use if-then-else or loop
-- constructs so that they only take effect under certain conditions.
--
-- Note: Atomic actions in the program are not split up, i.e., during the
-- execution of complex atomic actions no reactions are taken.
--
-- This construct is taken from (HOW TO CITE?).
monitor :: TestAction a => [Prog a] -> Prog a
monitor = foldl1' intersperse

-- | Injects the first program before every atom of the second program.
--
-- Much care is taken to solve the following problem: if the second program is
-- @p = Choice [t1 `Seq` ..., ...]@ and the first one is any program @q$, we do
-- not want to have a result @Choice [q `Seq` t1 `Seq` ..., ...]@. 
-- Why not? Because if t1 is, say, the branching condition of an if-then-else,
-- this test should be the head of the nondeterministic alternative, because
-- this allows the interpreter to never enter the branch if the test fails.
-- But if @q@ is added before, the interpreter will enter the tree. If @q@ is
-- long enough, even a large look-ahead won't avoid this.
--
-- Therefore the intended result is @q `Seq` Choice [t1 `Seq` ..., ...]@, and
-- for that we need 'nextPA' and 'finalP'.
intersperse :: Prog a -> Prog a -> Prog a
intersperse q p =
   case nextPA p of []                   -> Nil
                    [(c,p')] | finalP p  -> Choice
                                               Nil
                                               (q `Seq` Atom c `Seq`
                                                 intersperse q p')
                             | otherwise -> q `Seq` Atom c `Seq`
                                            intersperse q p'
                    ds       | finalP p  -> choice
                                               ( Nil
                                               : map (\(c,p') ->
                                                  q `Seq` Atom c `Seq`
                                                  intersperse q p') ds)
                             | otherwise -> q `Seq`
                                            choice (map (\(c,p') ->
                                               Atom c `Seq`
                                               intersperse q p') ds)

-- | Returns a list of decompositions of a 'Prog' into a next 'Atom' and a
-- remaining 'Prog'.
--
-- This function is slightly adapted from the old Golog interpreter
-- "Golog.Old.Interpreter". Here, we use it just to make atomic if-then-else
-- possible, and for interspersing programs.
nextPA :: Prog a -> [(Atom a, Prog a)]
nextPA (Seq p1 p2)    = map (\(c, p') -> (c, Seq p' p2)) (nextPA p1) ++
                        if finalP p1 then nextPA p2 else []
nextPA (Choice p1 p2) = nextPA p1 ++ nextPA p2
nextPA (Conc p1 p2)   = map (\(c, p') -> (c, Conc p' p2)) (nextPA p1) ++
                        map (\(c, p') -> (c, Conc p1 p')) (nextPA p2)
nextPA (Atom c)       = [(c, Nil)]
nextPA Nil            = []

-- | Indicates whether or not execution may stop for the given program.
--
-- This function is slightly adapted from the old Golog interpreter
-- "Golog.Old.Interpreter". Here, we use it just to make atomic if-then-else
-- possible, and for interspersing programs.
finalP :: Prog a -> Bool
finalP (Seq p1 p2)        = finalP p1 && finalP p2
finalP (Choice p1 p2)     = finalP p1 || finalP p2
finalP (Conc p1 p2)       = finalP p1 && finalP p2
finalP (Atom (Act _))     = False
finalP (Atom (Complex p)) = finalP p
finalP Nil                = True

