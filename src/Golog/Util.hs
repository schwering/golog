{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Golog.Util
  (trans', doo', exec) where

import Golog.Interpreter
import Control.Monad.State.Lazy

trans' :: Conf a (Node a b) -> Maybe (Conf a (Node a b))
trans' c = case trans c of []   -> Nothing
                           c':_ -> Just c'

doo' :: Conf a (Node a b) -> [Conf a (Node a b)]
doo' c = case trans' c of Nothing -> []
                          Just c' -> c' : doo' c'

-- toLists :: Tree a -> [[a]]
-- toLists Empty     = [[]]
-- toLists (Alt ts)  = concat (map toLists ts)
-- toLists (Val x t) = [x:xs | xs <- toLists t]

exec :: BAT a => a -> State (Maybe (Sit a)) ()
exec a = state f
   where f (Just s) | poss a s = ((), Just (do_ a s))
         f _                   = ((), Nothing)

instance Show a => Show (Atom a) where
   show (Prim a)  = "Prim " ++ show a
   show (PrimF _) = "PrimF <...>"
   show (Test _)  = "Test <...>"

{-
class ShowStopper a where
   stop :: a -> Bool

instance ShowStopper (Atom a) where
   stop _ = False

instance ShowStopper (Node a b) where
   stop (Node _ _) = False
   stop Flop       = True

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
-}

