{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Interpreter.Golog2Util where

import Interpreter.Golog2
import Control.Monad.State.Lazy

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

-- toLists :: Tree a -> [[a]]
-- toLists Empty     = [[]]
-- toLists (Alt ts)  = concat (map toLists ts)
-- toLists (Val x t) = [x:xs | xs <- toLists t]

exec :: BAT a => a -> State (Maybe (Sit a)) ()
exec a = state f
   where f (Just s) | poss a s = ((), Just (do_ a s))
         f _                   = ((), Nothing)

class ShowStopper a where
   stop :: a -> Bool

instance ShowStopper (Node a b) where
   stop (Node _ _) = False
   stop Flop       = True

instance ShowStopper (Atom a) where
   stop _ = False

instance Show a => Show (Atom a) where
   show (Prim a)  = "Prim " ++ show a
   show (PrimF _) = "PrimF <...>"
   show (Test _)  = "Test <...>"

instance (Show (Sit a), Show b) => Show (Node a b) where
   show (Node s b) = "Node" ++ " (" ++ show b ++ ") " ++ show s
   show Flop       = "Flop"

instance (Show (Sit a), Show b, ShowStopper b) => Show (Conf a b) where
   show (Conf t s) = "Conf (" ++ show s ++ ")\n" ++ show t

instance (Show a, ShowStopper a) => Show (Tree a) where
   show = showTree 5

showTree :: (Show a, ShowStopper a) => Int -> Tree a -> String
showTree d' = showTree' d' 0
   where showTree' 0 n _         = s n ++ "...\n"
         showTree' _ n Empty     = s n ++ "Empty\n"
         showTree' d n (Alt ts)  = s n ++ "Alt\n" ++ concat (map (showTree' (d-1) (n+1)) ts)
         showTree' d n (Val x t) = s n ++ "Val " ++ show x ++ "\n" ++
                                   ( if not (stop x) then showTree' d (n+1) t else "" )
         s n = replicate (2*n) ' '

