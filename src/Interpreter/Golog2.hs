{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE EmptyDataDecls #-}

module Interpreter.Golog2 where

import Prelude hiding (scanl)
import Control.Monad.State.Lazy
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
   lookahead  :: Sit a -> Depth

data Atom a = Prim a
            | PrimF (Sit a -> a)
            | Test (Sit a -> Bool)

data PseudoAtom a = Atom (Atom a)
                  | Complex (Prog a)

data Prog a where
   Seq        :: Prog a -> Prog a -> Prog a
   Nondet     :: [Prog a] -> Prog a
   Conc       :: Prog a -> Prog a -> Prog a
   Star       :: Prog a -> Prog a
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

scanl :: (b -> a -> b) -> b -> Tree a -> Tree b
scanl _ x Empty     = Val x Empty
scanl f x (Val y t) = Val x (scanl f (f x y) t)
scanl f x (Alt ts)  = Alt (map (scanl f x) ts)

best :: a -> (a -> a -> Ordering) -> (a -> Tree a -> Bool) -> Depth -> Tree a -> a
best def _   _   _ Empty                 = def
best def cmp cut l (Alt ts)              = maximumBy cmp (map (best def cmp cut l) ts)
best def cmp cut l (Val x t) | l == 0    = x
                             | cut x t   = maximumBy cmp [x, best def cmp cut (l-1) t]
                             | l > 0     = best def cmp cut (l-1) t
                             | otherwise = error "best: l < 0"

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
         den' (Star p)       = Alt (map den' (iterate (Seq p) Nil))
         den' (PseudoAtom a) = Val a Empty
         den' Nil            = Empty

trans :: BAT a => Sit a -> Tree (Atom a) -> [(Sit a, Tree (Atom a))]
trans _ Empty                        = []
trans s (Alt ts)                     = concat $ map (\t -> trans s t) ts
trans s (Val (Prim a)  t) | poss a s = [(do_ a s, t)]
trans s (Val (PrimF a) t)            = trans s (Val (Prim (a s)) t)
trans s (Val (Test f)  t) | f s      = [(s, t)]
trans _ (Val _         _)            = []

treeDT :: DTBAT a => Sit a -> Prog a -> Tree (Maybe (Sit a, Reward, Depth))
treeDT sz p = scanl augment root (den p)
   where root = Just (sz, 0, 0)
         augment :: DTBAT a => Maybe (Sit a, Reward, Depth) -> Atom a -> Maybe (Sit a, Reward, Depth)
         augment (Just (s,r,d)) (Prim a)  | poss a s = Just (do_ a s, r + reward a s, d + 1)
         augment (Just (s,r,d)) (PrimF a)            = augment (Just (s,r,d)) (Prim (a s))
         augment (Just (s,r,d)) (Test f)  | f s      = Just (s, r, d + 1)
         augment _              _                    = Nothing

transDT :: DTBAT a => Sit a -> Tree (Sit a, Reward, Depth) -> Maybe (Sit a, Tree (Sit a, Reward, Depth))
transDT _ Empty           = Nothing
transDT _ (Val (s,_,_) t) = Just (s, t)
--transDT _ (Alt [])        = Nothing
transDT s (Alt ts)        = maximumBy (comparing (value (lookahead s))) ts
   where value :: DTBAT a => Depth -> Tree (Maybe (Sit a, Reward, Depth)) -> (Reward, Depth)
         value l t = val (best def cmp fnl l t)
            where val :: (Num b, Num c, Ord b, Ord c) => Maybe (a, b, c) -> (b, c)
                  val (Just (_, r, d)) = (r, d)
                  val Nothing          = (0, 0)
                  def             = Just (s0, 0, 0)
                  fnl _ Empty     = True
                  fnl _ (Alt ts)  = any (\t -> case t of Empty -> True ; _ -> False) ts
                  fnl _ (Val _ _) = False
                  cmp :: (Num b, Num c, Ord b, Ord c) => Maybe (a, b, c) -> Maybe (a, b, c) -> Ordering
                  cmp x y         = compare (val x) (val y)

exec :: BAT a => a -> State (Maybe (Sit a)) ()
exec a = state f
   where f (Just s) | poss a s = ((), Just (do_ a s))
         f _                   = ((), Nothing)

