{-# LANGUAGE GADTs, TypeFamilies #-}

module Interpreter.Golog2
  (BAT(..), DTBAT(..), Reward, Depth,
   Atom(..), PseudoAtom(..), Prog(..), Tree(..), Node(..), Conf(..),
   sit, treeND, treeDT, trans, trans', final, final', do1, do2) where

import Data.List (maximumBy)
import Data.Monoid
import Data.Ord (comparing)

type Reward = Double
type Depth = Int

class BAT a where
   data Sit a :: *
   s0         :: Sit a
   do_        :: a -> Sit a -> Sit a
   poss       :: a -> Sit a -> Bool

class BAT a => DTBAT a where
   reward     :: a -> Sit a -> Reward

data Atom a = Prim a | PrimF (Sit a -> a) | Test (Sit a -> Bool)

data PseudoAtom a = Atom (Atom a) | Complex (Prog a)

data Prog a where
   Seq        :: Prog a -> Prog a -> Prog a
   Nondet     :: [Prog a] -> Prog a
   Conc       :: Prog a -> Prog a -> Prog a
   PseudoAtom :: PseudoAtom a -> Prog a
   Nil        :: Prog a

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

scan :: (b -> a -> b) -> b -> Tree a -> Tree b
scan _ _ Empty     = Empty
scan f x (Val y t) = let z = f x y in Val z (scan f z t)
scan f x (Alt ts)  = Alt (map (scan f x) ts)

best :: a -> (a -> a -> Ordering) -> (Tree a -> Bool) -> Depth -> Tree a -> a
best def _   _   _ Empty                 = def
best def cmp cut l (Alt ts)              = maximumBy cmp (map (best def cmp cut l) ts)
best def cmp cut l (Val x t) | l == 0    = x
                             | cut t     = maximumBy cmp [x, best def cmp cut (l-1) t]
                             | l > 0     = best def cmp cut (l-1) t
                             | otherwise = error "best: l < 0"

resolve :: ([Tree a] -> Tree a) -> Tree a -> Tree a
resolve _ Empty     = Empty
resolve _ (Alt [])  = Empty
resolve f (Alt ts)  = resolve f (f ts)
resolve f (Val x t) = Val x (resolve f t)

itl :: Tree a -> Tree a -> Tree a
itl Empty          t2             = t2
itl t1             Empty          = t1
itl (Alt ts)       t2             = Alt (map (\t1 -> itl t1 t2) ts)
itl t1             (Alt ts)       = Alt (map (\t2 -> itl t1 t2) ts)
itl t1@(Val x1 r1) t2@(Val x2 r2) = Alt [Val x1 (itl t2 r1), Val x2 (itl t1 r2)]

den :: Prog a -> Tree (Atom a)
den p' = rec (den' p')
   where rec :: Tree (PseudoAtom a) -> Tree (Atom a)
         rec Empty               = Empty
         rec (Alt ts)            = Alt (map rec ts)
         rec (Val (Complex p) t) = mappend (den p) (rec t)
         rec (Val (Atom a)    t) = Val a (rec t)
         den' :: Prog a -> Tree (PseudoAtom a)
         den' (Seq p1 p2)    = mappend (den' p1) (den' p2)
         den' (Nondet ps)    = Alt (map den' ps)
         den' (Conc p1 p2)   = itl (den' p1) (den' p2)
         den' (PseudoAtom a) = Val a Empty
         den' Nil            = Empty

data Conf a b = Conf (Tree b) (Sit a)

data Node a b = Node (Sit a) b | Flop

type NodeN a = Node a ()
type NodeDT a = Node a (Reward, Depth)

sit :: Conf a b -> Sit a
sit (Conf _ s) = s

treeND :: BAT a => Prog a -> Sit a -> Conf a (NodeN a)
treeND p sz = Conf (scan exec (Node sz ()) (den p)) sz
   where exec :: BAT a => NodeN a -> Atom a -> NodeN a
         exec (Node s _)   (Prim a)  | poss a s = Node (do_ a s) ()
         exec c@(Node s _) (PrimF a)            = exec c (Prim (a s))
         exec c@(Node s _) (Test f)  | f s      = c
         exec _            _                    = Flop

treeDT :: DTBAT a => Depth -> Prog a -> Sit a -> Conf a (NodeDT a)
treeDT l p sz = Conf (resolve choice (scan exec (Node sz (0,0)) (den p))) sz
   where exec :: DTBAT a => NodeDT a -> Atom a -> NodeDT a
         exec (Node s (r,d)) (Prim a)  | poss a s = Node (do_ a s)
                                                         (r + reward a s, d+1)
         exec c@(Node s _)   (PrimF a)            = exec c (Prim (a s))
         exec (Node s (r,d)) (Test f)  | f s      = Node s (r,d+1)
         exec _              _                    = Flop
         choice = maximumBy (comparing (value l))
         value :: DTBAT a => Depth -> Tree (NodeDT a) -> (Reward, Depth)
         value l' = val . best def cmp final' l'
            where def = Node s0 (0,0)
                  val (Node _ rd) = rd
                  val Flop        = (0,0)
                  cmp x y = compare (val x) (val y)

trans :: Conf a (Node a b) -> [Conf a (Node a b)]
trans (Conf Empty              _) = []
trans (Conf (Val (Node s _) t) _) = [Conf t s]
trans (Conf (Val Flop       _) _) = []
trans (Conf (Alt ts)           s) = concat (map (\t -> trans (Conf t s)) ts)

trans' :: Conf a (Node a b) -> Maybe (Conf a (Node a b))
trans' c = case trans c of []   -> Nothing
                           c':_ -> Just c'

final  :: Conf a b -> Bool
final (Conf t _) = final' t

final' :: Tree a -> Bool
final' Empty     = True
final' (Alt [])  = True
final' (Alt ts)  = any final' ts
final' (Val _ _) = False

do1 :: Conf a (Node a b) -> [[Conf a (Node a b)]]
do1 c = let cs = trans c in cs : concat (map do1 cs)

do2 :: Conf a (Node a b) -> [Conf a (Node a b)]
do2 c = case trans' c of Nothing -> []
                         Just c' -> c' : do2 c'

