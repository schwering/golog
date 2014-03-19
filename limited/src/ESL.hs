{-# LANGUAGE GADTs, TypeFamilies, MultiParamTypeClasses, FlexibleContexts, EmptyDataDecls #-}

module ESL
  () where

import Data.List

type VarSymbol = Int
type PredSymbol = Int

data Var a = Var VarSymbol

data Nonground a = NGVar a | NGStdName a
data Ground a = GStdName a

data EqCond f a g b where
   ETrue   :: (StdName a, StdName b) => EqCond f a g b
   EEqualO :: (StdName a, StdName b) => Nonground a -> Nonground a -> EqCond f a Nonground b
   EEqualA :: (StdName a, StdName b) => Nonground b -> Nonground b -> EqCond Nonground a g b
   ENeg    :: (StdName a, StdName b) => EqCond f a g b -> EqCond f a g b
   EAnd    :: (StdName a, StdName b) => EqCond f a g b -> EqCond f a g b -> EqCond f a g b

data ExtLit f a g b where
   ExtLit :: (StdName a, StdName b) => [f a] -> Bool -> PredSymbol -> [g b] -> ExtLit f a g b

type Clause f a g b = [ExtLit f a g b]

data BoxForm f a g b where
   BoxForm :: (StdName a, StdName b) => EqCond f a g b -> Clause f a g b -> BoxForm f a g b

data BoundedForm f a g b where
   BoundedForm :: (StdName a, StdName b) => EqCond f a g b -> Clause f a g b -> BoundedForm f a g b

class (Eq a, Ord a) => StdName a where
   hplus :: Int -> [a] -> [a]

class (StdName a, StdName b) => ProperBAT a b where
   data Action a :: *
   data Object b :: *
   bat :: [BoxForm f a g b]

data Query a g b where
   QAtom   :: PredSymbol -> [g b] -> Query a g b
   QEqualO :: b -> b -> Query a g b
   QNeg    :: Query a g b -> Query a g b
   QOr     :: Query a g b -> Query a g b -> Query a g b
   QExists :: Var b -> Query a Nonground b -> Query a Nonground b
   QAction :: a -> Query a g b -> Query a g b

as :: StdName a => Query a g b -> [[a]]
as (QAtom _ _)      = [[]]
as (QEqualO _ _)    = [[]]
as (QNeg phi)       = as phi
as (QOr phi1 phi2)  = as phi1 ++ as phi2
as (QExists _ phi)  = as phi
as (QAction a phi)  = [a:z | z <- as phi]

asz :: StdName a => [a] -> Query a g b -> [[a]]
asz z phi = [z ++ z' | z' <- as phi]

pel :: (StdName a, StdName b) => [BoxForm f a g b] -> Query a g b -> [[a]] -> [ExtLit Ground a g b]
pel bat phi zz = [ExtLit (map GStdName z) True p args | (p, args) <- concat (map pel1 bat) ++ pel2 phi, z <- concat $ inits zz]
   where pel1 (BoxForm _ c)   = map (\(ExtLit _ _ p args) -> (p, args)) c
         pel2 (QAtom p args)  = [(p, args)]
         pel2 (QEqualO _ _)   = []
         pel2 (QNeg _)        = []
         pel2 (QOr phi1 phi2) = pel2 phi1 ++ pel2 phi2
         pel2 (QExists _ phi) = pel2 phi
         pel2 (QAction _ phi) = pel2 phi

