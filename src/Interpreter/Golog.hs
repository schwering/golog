-- | Golog interpreter with decision theory and concurrency.
-- Nondeterministic construct like branch, iteration, pick, and concurrency by
-- interleaving are resolved by opting for the choice that leads to the highest
-- rewarded situation.
--
-- The semantics is based on [paper], but not that close to the formal
-- definition:
--
-- * Programs are decomposed into a next atomic action and the respective
--   remaining program. This is done by the functions private functions @next@
--   and and @next'@ as in [1].
--
-- * The tree of reachable situations is constructed (lazily). This is done by
--   decomposing the program, executing the next atomic action, and continuing
--   this recursively for the remaining program.
--   In this tree final configurations are represented as leaves.
--   This is done by 'tree'.
--
-- \[1\] http:\/\/www.aaai.org\/ocs\/index.php\/WS\/AAAIW12\/paper\/view\/5281

{-# LANGUAGE ExistentialQuantification #-}

module Interpreter.Golog (Sit(S0, Do), Reward, Depth, MaxiF,
                          Atom(..), PseudoAtom(..), Prog(..), SitTree,
                          BAT(..),
                          tree, trans, final, value) where

import Interpreter.Tree

data Sit a = S0
           | Do a (Sit a)

type Reward = Double
type Depth = Int

type MaxiF b = (b -> (Reward, Depth)) -> b

data Atom a = Prim a
            | PrimF (Sit a -> a)
            | Test (Sit a -> Bool)

data PseudoAtom a = Atom (Atom a)
                  | Complex (Prog a)

data Prog a = Seq (Prog a) (Prog a)
            | Nondet (Prog a) (Prog a)
            | Conc (Prog a) (Prog a)
            | Star (Prog a)
            | forall b. Pick (MaxiF b) b (b -> Prog a)
            | PseudoAtom (PseudoAtom a)
            | Nil


type SitTree a = Tree (Reward, Depth) (Sit a, Reward, Depth)


class BAT a where
   poss :: a -> Sit a -> Bool
   reward :: a -> Sit a -> Reward


type PseudoDecomp a = (PseudoAtom a, Prog a)
type Decomp a = (Atom a, Prog a)


next :: Prog a -> Tree (Reward, Depth) (PseudoDecomp a)
next (Seq p1 p2)    = let t1 = fmap (\(c, r) -> (c, Seq r p2)) (next p1)
                      in if finalP p1 then branch (next p2) t1 else t1
next (Nondet p1 p2) = branch (next p1) (next p2)
next (Conc p1 p2)   = branch (fmap (\(c, r) -> (c, Conc r p2)) (next p1))
                             (fmap (\(c, r) -> (c, Conc p1 r)) (next p2))
next (Pick g _ p)   = Sprout g (\x -> next (p x))
next (Star p)       = fmap (\(c, r) -> (c, Seq r (Star p))) (next p)
next (PseudoAtom c) = Leaf (c, Nil)
next Nil            = Empty


finalP :: Prog a -> Bool
finalP (Seq p1 p2)              = finalP p1 && finalP p2
finalP (Nondet p1 p2)           = finalP p1 || finalP p2
finalP (Conc p1 p2)             = finalP p1 && finalP p2
finalP (Pick _ x0 p)            = finalP (p x0)
finalP (Star _)                 = True
finalP (PseudoAtom (Atom _))    = False
finalP (PseudoAtom (Complex p)) = finalP p
finalP Nil                      = True


next' :: Prog a -> Tree (Reward, Depth) (Decomp a)
next' p = lmap h (next p)
   where h ((Atom c), r)     = Leaf (c, r)
         h ((Complex p'), r) = next' (Seq p' r)


tree :: (BAT a) => Prog a -> Sit a -> Reward -> Depth -> SitTree a
tree p s v d = let t1 = Leaf (s, v, d)
                   t2 = lmap transAtom (next' p)
               in if finalP p then branch t1 t2 else t2
   where transAtom (Prim a, r)  | poss a s  = let s' = Do a s
                                                  v' = v + (reward a s)
                                                  d' = d + 1
                                              in Parent (s', v', d')
                                                        (tree r s' v' d')
                                | otherwise = Empty
         transAtom (PrimF b, r)             = transAtom (Prim (b s), r)
         transAtom (Test t, r)  | t s       = tree r s v (d+1)
                                | otherwise = Empty


pickbest :: Depth -> SitTree a -> SitTree a
pickbest d = force (value d) (-1/0, minBound)


value :: Depth -> SitTree a -> (Reward, Depth)
value _ t @ Empty        = quality t
value _ t @ (Leaf _)     = quality t
value d t @ (Parent (_, _, d') t')
             | d > d'    = value d t'
             | d == d'   = max (quality t) (value d t')
             | otherwise = quality Empty
value d (Branch t1 t2)   = max (value d t1) (value d t2)
value _ (Sprout _ _)     = error "Golog.value: Sprout"


quality :: SitTree a -> (Reward, Depth)
quality Empty                = (0.0, 0)
quality (Leaf (_, r, d))     = (r, d)
quality (Parent (_, r, d) _) = (r, d)
quality (Branch _ _)         = error "Golog.quality: Branch"
quality (Sprout _ _)         = error "Golog.quality: Sprout"


trans :: Depth -> SitTree a -> SitTree a
trans _ t @ Empty         = t
trans _ t @ (Leaf _)      = t
trans d t @ (Parent _ t') = if value d t >= value d t' then t else trans d t'
trans d (Branch t1 t2)    = trans d (if value d t1 >= value d t2 then t1 else t2)
trans _ (Sprout _ _)      = error "Golog.trans: Sprout"


final :: SitTree a -> Bool
final Empty        = True
final (Leaf _)     = True
final (Parent _ _) = False
final (Branch _ _) = False
final (Sprout _ _) = error "Golog.final: Sprout"

