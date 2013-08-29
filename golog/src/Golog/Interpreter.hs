{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module Golog.Interpreter
  (BAT(..), DTBAT(..), IOBAT(..),
   Atom(..), PseudoAtom(..), Prog(..), Conf, ConfIO, Depth,
   treeND, treeDT, treeNDIO, treeDTIO, final, trans, sit, sync) where

import Control.Monad (liftM)
import Data.List (maximumBy)
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

data Atom a       = Prim a | PrimF (Sit a -> a)
data PseudoAtom a = Atom (Atom a) | Complex (Prog a)
data Prog a       = Seq (Prog a) (Prog a)  | Nondet [Prog a]
                  | Conc (Prog a) (Prog a) | PseudoAtom (PseudoAtom a) | Nil

type Depth = Int

data Tree a = Empty | Alt [Tree a] | Val a (Tree a)

instance Monoid (Tree a) where
   mempty                = Empty
   mappend Empty      t2 = t2
   mappend (Alt ts)   t2 = Alt (fmap (\t1 -> mappend t1 t2) ts)
   mappend (Val x t1) t2 = Val x (mappend t1 t2)

instance Functor Tree where
   fmap _ Empty     = Empty
   fmap f (Alt ts)  = Alt (map (fmap f) ts)
   fmap f (Val x t) = Val (f x) (fmap f t)

scan :: (b -> a -> Tree a -> b) -> b -> Tree a -> Tree b
scan f x0 t0 = Val x0 (scan' x0 t0)
   where scan' _ Empty     = Empty
         scan' x (Val y t) = let z = f x y t in Val z (scan' z t)
         scan' x (Alt ts)  = Alt (map (scan' x) ts)

best :: (a -> a -> Ordering) -> (Tree a -> Bool) -> Depth -> a -> Tree a -> a
best _   _   _ x Empty     = x
best _   _   _ x (Alt [])  = x
best cmp cut l x (Alt ts)  = maximumBy cmp (map (best cmp cut l x) ts)
best cmp cut l x (Val y t) | l == 0    = y
                           | cut t     = best cmp cut (l-1) y t
                           | l > 0     = best cmp cut (l-1) x t
                           | otherwise = error "best: l < 0"

resolve :: (a -> [Tree a] -> Tree a) -> a -> Tree a -> Tree a
resolve _ _ Empty     = Empty
resolve _ _ (Alt [])  = Empty
resolve f x (Alt ts)  = resolve f x (f x ts)
resolve f _ (Val x t) = Val x (resolve f x t)

itl :: Tree a -> Tree a -> Tree a
itl Empty          t2             = t2
itl t1             Empty          = t1
itl (Alt ts)       t2             = Alt (map (\t1 -> itl t1 t2) ts)
itl t1             (Alt ts)       = Alt (map (\t2 -> itl t1 t2) ts)
itl t1@(Val x1 r1) t2@(Val x2 r2) = Alt [Val x1 (itl t2 r1), Val x2 (itl t1 r2)]

ast :: Prog a -> Tree (Atom a)
ast p' = rec (ast' p')
   where rec :: Tree (PseudoAtom a) -> Tree (Atom a)
         rec Empty               = Empty
         rec (Alt ts)            = Alt (map rec ts)
         rec (Val (Atom a)    t) = Val a (rec t)
         rec (Val (Complex p) t) = mappend (ast p) (rec t)
         ast' :: Prog a -> Tree (PseudoAtom a)
         ast' (Seq p1 p2)    = mappend (ast' p1) (ast' p2)
         ast' (Nondet ps)    = Alt (map ast' ps)
         ast' (Conc p1 p2)   = itl (ast' p1) (ast' p2)
         ast' (PseudoAtom a) = Val a Empty
         ast' Nil            = Empty

data Node a b = Node (Sit a) b | Flop
type Conf a b = Tree (Node a b)
type ConfIO a m = Conf a (SyncIO a m)
newtype SyncIO a m = SyncIO { runSync :: m (ConfIO a m) }

treeND :: BAT a => Prog a -> Sit a -> Conf a ()
treeND p sz = scan (exec (\_ _ _ -> ())) (Node sz ()) (ast p)

treeDT :: DTBAT a => Depth -> Prog a -> Sit a -> Conf a ()
treeDT l p sz = resolveDT l (treeND p sz)

treeNDIO :: IOBAT a m => Prog a -> Sit a -> ConfIO a m
treeNDIO p sz = cnf sz (ast p)
   where cnf s t = scan (exec f) (root s t) t
         root s t = Node s (SyncIO $ return (cnf s t))
         f pl a t = (SyncIO $ do c <- runSync pl
                                 s' <- syncA a (sit c)
                                 return (cnf s' t))

treeDTIO :: (DTBAT a, IOBAT a m) => Depth -> Prog a -> Sit a -> ConfIO a m
treeDTIO l p sz = f (treeNDIO p sz)
   where f = fmap g . resolveDT l
         g (Node s (SyncIO c)) = Node s (SyncIO $ liftM f c)
         g Flop                = Flop

exec :: BAT a => (b -> a -> Tree (Atom a) -> b) ->
                 Node a b -> Atom a -> Tree (Atom a) -> Node a b
exec f (Node s pl)  (Prim a)  t | poss a s = Node (do_ a s) (f pl a t)
exec f c@(Node s _) (PrimF a) t            = exec f c (Prim (a s)) t
exec _ _            _         _            = Flop

resolveDT :: DTBAT a => Depth -> Conf a b -> Conf a b
resolveDT l = resolve chooseDT Flop
   where chooseDT def = maximumBy (comparing value)
            where value t = val (best cmp final l def t)
                  val (Node s _) = Just (reward s)
                  val Flop       = Nothing
                  cmp x y        = compare (val x) (val y)

final :: Conf a b -> Bool
final = final' True
   where final' _    Empty     = True
         final' _    (Alt [])  = True
         final' next (Alt ts)  = any (final' next) ts
         final' next (Val _ t) = next && final' False t

trans :: Conf a b -> [Conf a b]
trans Empty              = error "trans: invalid conf"
trans (Alt _)            = error "trans: invalid conf"
trans (Val Flop       _) = []
trans (Val (Node _ _) t) = trans' t
   where trans' Empty                 = []
         trans' (Alt ts)              = concat (map trans' ts)
         trans' (Val Flop          _) = []
         trans' t'@(Val (Node _ _) _) = [t']

sit :: Conf a b -> Sit a
sit (Val (Node s _) _) = s
sit _                  = error "sit: invalid conf"

sync :: ConfIO a m -> m (ConfIO a m)
sync (Val (Node _ pl) _) = runSync pl
sync _                   = error "sync: invalid conf"

