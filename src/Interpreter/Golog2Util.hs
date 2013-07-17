{-# LANGUAGE GADTs #-}

module Interpreter.Golog2Util where

import Interpreter.Golog2
import Control.Monad.State.Lazy

-- toLists :: Tree a -> [[a]]
-- toLists Empty     = [[]]
-- toLists (Alt ts)  = concat (map toLists ts)
-- toLists (Val x t) = [x:xs | xs <- toLists t]

star :: Prog a -> Prog a
star p = Nondet [Nil, plus p]

plus :: Prog a -> Prog a
plus p = Nondet [p, p `Seq` plus p]

ifThenElse :: (Sit a -> Bool) -> Prog a -> Prog a -> Prog a
ifThenElse phi p1 p2 = Nondet [ t phi       `Seq` p1
                              , t (not.phi) `Seq` p2 ]
   where t = PseudoAtom . Atom . Test

exec :: BAT a => a -> State (Maybe (Sit a)) ()
exec a = state f
   where f (Just s) | poss a s = ((), Just (do_ a s))
         f _                   = ((), Nothing)

