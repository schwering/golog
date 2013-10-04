{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module Golog.Interpreter
  (BAT(..), DTBAT(..), IOBAT(..),
   Atom(..), Prog(..), Conf, Sync,
   treeND, treeDT, treeNDIO, treeDTIO, final, trans, sit, sync) where

import Control.Monad (liftM)
import Data.Monoid (Monoid(..))
import Data.Ord (comparing)

class BAT a where
   data Sit a    :: *
   s0            :: Sit a
   do_           :: a -> Sit a -> Sit a
   poss          :: a -> Sit a -> Bool

class (BAT a, Ord (Reward a)) => DTBAT a where
   data Reward a :: *
   reward        :: Sit a -> Reward a

class (BAT a, Monad m) => IOBAT a m where
   syncA         :: a -> Sit a -> m (Sit a)

data Atom a = Act (Sit a -> a) | Complex (Prog a)
data Prog a = Seq (Prog a) (Prog a)  | Choice (Prog a) (Prog a)
            | Conc (Prog a) (Prog a) | Atom (Atom a) | Nil

data Tree a = Empty | Alt (Tree a) (Tree a) | Val a (Tree a)

instance Monoid (Tree a) where
   mempty                = Empty
   mappend Empty       t = t
   mappend (Alt t1 t2) t = Alt (mappend t1 t) (mappend t2 t)
   mappend (Val x t')  t = Val x (mappend t' t)

instance Functor Tree where
   fmap _ Empty       = Empty
   fmap f (Alt t1 t2) = Alt (fmap f t1) (fmap f t2)
   fmap f (Val x t)   = Val (f x) (fmap f t)

scan :: (b -> a -> Tree a -> b) -> b -> Tree a -> Tree b
scan f x0 t0 = Val x0 (scan' x0 t0)
   where scan' _ Empty       = Empty
         scan' x (Alt t1 t2) = Alt (scan' x t1) (scan' x t2)
         scan' x (Val y t)   = let z = f x y t in Val z (scan' z t)

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy cmp x y = case cmp x y of GT -> x
                                _  -> y

best :: (a -> a -> Ordering) -> (Tree a -> Bool) -> Int -> a -> Tree a -> a
best _   _   _ x Empty       = x
best cmp cut l x (Alt t1 t2) = maxBy cmp (best cmp cut l x t1)
                                         (best cmp cut l x t2)
best cmp cut l x (Val y t) | l == 0    = y
                           | cut t     = best cmp cut (l-1) y t
                           | l > 0     = best cmp cut (l-1) x t
                           | otherwise = error "best: l < 0"

choose :: (a -> Tree a -> Tree a -> Tree a) -> a -> Tree a -> Tree a
choose _ _ Empty       = Empty
choose f x (Alt t1 t2) = choose f x (f x t1 t2)
choose f _ (Val x t)   = Val x (choose f x t)

itl :: Tree a -> Tree a -> Tree a
itl Empty          t              = t
itl t              Empty          = t
itl (Alt t1 t2)    t              = Alt (itl t1 t) (itl t2 t)
itl t              (Alt t1 t2)    = Alt (itl t t1) (itl t t2)
itl t1@(Val x1 r1) t2@(Val x2 r2) = Alt (Val x1 (itl t2 r1))
                                        (Val x2 (itl t1 r2))

nf :: Prog a -> Tree (Sit a -> a)
nf p' = rec (nf' p')
   where rec :: Tree (Atom a) -> Tree (Sit a -> a)
         rec Empty               = Empty
         rec (Alt t1 t2)         = Alt (rec t1) (rec t2)
         rec (Val (Act a)     t) = Val a (rec t)
         rec (Val (Complex p) t) = mappend (nf p) (rec t)
         nf' :: Prog a -> Tree (Atom a)
         nf' (Seq p1 p2)    = mappend (nf' p1) (nf' p2)
         nf' (Choice p1 p2) = Alt (nf' p1) (nf' p2)
         nf' (Conc p1 p2)   = itl (nf' p1) (nf' p2)
         nf' (Atom a)       = Val a Empty
         nf' Nil            = Empty

data Node a b = Node (Sit a) b | Flop
type Conf a b = Tree (Node a b)
newtype Sync a m = Sync { runSync :: m (Conf a (Sync a m)) }

treeND :: BAT a => Prog a -> Sit a -> Conf a ()
treeND p sz = scan e (Node sz ()) (nf p)
   where e (Node s _) a _ | poss (a s) s = Node (do_ (a s) s) ()
         e _          _ _                = Flop

treeDT :: DTBAT a => Int -> Prog a -> Sit a -> Conf a ()
treeDT l = (resolveDT l .) . treeND

treeNDIO :: IOBAT a m => Prog a -> Sit a -> Conf a (Sync a m)
treeNDIO = cnf . nf
   where cnf t s = scan e (Node s (Sync $ return (cnf t s))) t
         e (Node s pl) a t | poss (a s) s = Node (do_ (a s) s) (Sync $ do
                                                     c <- runSync pl
                                                     s' <- syncA (a s) (sit c)
                                                     return (cnf t s'))
         e _           _ _                = Flop

treeDTIO :: (DTBAT a, IOBAT a m) => Int -> Prog a -> Sit a -> Conf a (Sync a m)
treeDTIO l = (f .) . treeNDIO
   where f = fmap g . resolveDT l
         g (Node s (Sync c)) = Node s (Sync $ liftM f c)
         g Flop              = Flop

resolveDT :: DTBAT a => Int -> Conf a b -> Conf a b
resolveDT l = choose dt Flop
   where dt def = maxBy (comparing value)
            where value t = val (best cmp final l def t)
                  val (Node s _) = Just (reward s)
                  val Flop       = Nothing
                  cmp x y        = compare (val x) (val y)

final :: Conf a b -> Bool
final = final' True
   where final' _    Empty       = True
         final' next (Alt t1 t2) = final' next t1 || final' next t2
         final' next (Val _ t)   = next && final' False t

trans :: Conf a b -> [Conf a b]
trans Empty              = error "trans: invalid conf"
trans (Alt _ _)          = error "trans: invalid conf"
trans (Val Flop       _) = []
trans (Val (Node _ _) t) = trans' t
   where trans' Empty                 = []
         trans' (Alt t1 t2)           = trans' t1 ++ trans' t2
         trans' (Val Flop          _) = []
         trans' t'@(Val (Node _ _) _) = [t']

sit :: Conf a b -> Sit a
sit (Val (Node s _) _) = s
sit _                  = error "sit: invalid conf"

sync :: Conf a (Sync a m) -> m (Conf a (Sync a m))
sync (Val (Node _ pl) _) = runSync pl
sync _                   = error "sync: invalid conf"

