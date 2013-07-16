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
   deriving Show

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

resolve :: ([Tree a] -> Tree a) -> Tree a -> Tree a
resolve _ Empty     = Empty
resolve f (Val x t) = Val x (resolve f t)
resolve _ (Alt [])  = Empty
resolve f (Alt ts)  = f ts

best :: a -> (a -> a -> Ordering) -> (Tree a -> Bool) -> Depth -> Tree a -> a
best def _   _   _ Empty                 = def
best def cmp cut l (Alt ts)              = maximumBy cmp (map (best def cmp cut l) ts)
best def cmp cut l (Val x t) | l == 0    = x
                             | cut t     = maximumBy cmp [x, best def cmp cut (l-1) t]
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

data Conf a b = Conf (Tree b) (Sit a)

instance Show b => Show (Conf a b) where
   show (Conf t s) = show t

type NodeN a = Maybe (Sit a)
type NodeDT a = Maybe (Sit a, Reward, Depth)

tree :: BAT a => Prog a -> Sit a -> Conf a (NodeN a)
tree p sz = Conf (scanl exec (Just sz) (den p)) sz
   where exec :: BAT a => NodeN a -> Atom a -> NodeN a
         exec (Just s) (Prim a)  | poss a s = Just (do_ a s)
         exec (Just s) (PrimF a)            = exec (Just s) (Prim (a s))
         exec (Just s) (Test f)  | f s      = Just s
         exec _        _                    = Nothing

treeDT :: DTBAT a => Prog a -> Sit a -> Conf a (NodeDT a)
treeDT p sz = Conf (resolve choice (scanl exec (Just (sz,0,0)) (den p))) sz
   where exec :: DTBAT a => NodeDT a -> Atom a -> NodeDT a
         exec (Just (s,r,d))   (Prim a)  | poss a s = let s' = do_ a s
                                                          r' = reward a s
                                                          d' = d+1
                                                      in Just (s',r',d')
         exec c@(Just (s,_,_)) (PrimF a)            = exec c (Prim (a s))
         exec (Just (s,r,d))   (Test f)  | f s      = Just (s,r,d+1)
         exec _                _                    = Nothing
         choice = maximumBy (comparing (value (lookahead sz)))
         value :: DTBAT a => Depth -> Tree (NodeDT a) -> (Reward, Depth)
         value l t = val $ best def cmp final' l t
            where def = Just (s0,0,0)
                  val (Just (_,r,d)) = (r,d)
                  val Nothing        = (0,0)
                  cmp x y = compare (val x) (val y)

trans :: BAT a => Conf a (NodeN a) -> [Conf a (NodeN a)]
trans (Conf Empty             _) = []
trans (Conf (Val Nothing   _) _) = []
trans (Conf (Val (Just s') t) _) = [Conf t s']
trans (Conf (Alt ts)          s) = concat $ map (\t -> trans (Conf t s)) ts

transDT :: BAT a => Conf a (NodeDT a) -> [Conf a (NodeDT a)]
transDT (Conf Empty             _)       = []
transDT (Conf (Val Nothing   _) _)       = []
transDT (Conf (Val (Just (s',_,_)) t) _) = [Conf t s']
transDT (Conf (Alt ts)          s)       = concat $ map (\t -> transDT (Conf t s)) ts

trans' :: BAT a => Conf a (NodeN a) -> Maybe (Conf a (NodeN a))
trans' c = case trans c of []   -> Nothing
                           c':_ -> Just c'

final  :: Conf a b -> Bool
final (Conf t _) = final' t

final' :: Tree a -> Bool
final' Empty     = True
final' (Alt [])  = True
final' (Alt ts)  = any final' ts
final' (Val _ _) = False

{-
exec :: BAT a => a -> State (Maybe (Sit a)) ()
exec a = state f
   where f (Just s) | poss a s = ((), Just (do_ a s))
         f _                   = ((), Nothing)
-}

instance BAT Int where
   data Sit Int = S0 | Do Int (Sit Int) deriving Show
   s0 = S0
   do_ = Do
   poss a _ = even a

instance DTBAT Int where
   reward a _ = fromIntegral a
   lookahead _ = 2

p = PseudoAtom . Atom . Prim
q = PseudoAtom . Complex

-- let pp = Nondet [p 2 `Seq` p 4, p 6, p 0 `Seq` p 0 `Seq` p 10] :: Prog Int ; cc = treeDT pp s0 ; sit (Conf _ s) = s in mapM_ (putStrLn.show) $ take 90 $ map sit $ concat $ iterate (concat . map transDT) [cc]

