{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Golog.Util
   where

import Data.Maybe (listToMaybe)
import qualified Data.Tree as T
--import Control.Monad.State.Lazy
import Golog.Interpreter

class BAT a => HistBAT a where
   predSit :: Sit a -> Maybe (a, Sit a)

trans' :: Conf a b -> Maybe (Conf a b)
trans' = listToMaybe . trans

-- | Returns a list of lists of configurations where the @n@-th list of
-- configuration is the result of applying 'trans' to the configurations in the
-- @n-1@-th list.
transStar :: Conf a b -> [[Conf a b]]
transStar c = takeWhile (not.null) $ iterate (concatMap trans) [c]

-- Returns a list of lists of configurations where each list contains a sequence
-- of configurations each of which is a result of applying 'trans' to its
-- predecessor.
-- For some reason, this function has some issues with lazy evaluation.
transTrace :: Conf a b -> T.Tree (Conf a b)
transTrace c = T.Node c (map transTrace (trans c))

transTrace' :: Conf a b -> [Conf a b]
transTrace' c = c : case trans' c of Just c' -> transTrace' c'
                                     Nothing -> []

-- | Returns the list of final configurations reached from the given one.
doo :: Conf a b -> [Conf a b]
doo c = concat $ map (filter final) (transStar c)

doo' :: Conf a b -> Maybe (Conf a b)
doo' = listToMaybe . doo

-- toLists :: Tree a -> [[a]]
-- toLists Empty     = [[]]
-- toLists (Alt ts)  = concat (map toLists ts)
-- toLists (Val x t) = [x:xs | xs <- toLists t]

{-
exec :: BAT a => a -> State (Maybe (Sit a)) ()
exec a = state f
   where f (Just s) | poss a s = ((), Just (do_ a s))
         f _                   = ((), Nothing)
-}

instance Show a => Show (Atom a) where
   show (Prim a)  = "Prim " ++ show a
   show (PrimF _) = "PrimF <...>"
   show (Test _)  = "Test <...>"

instance Show a => Show (PseudoAtom a) where
   show (Atom a)    = show a
   show (Complex a) = "Complex (" ++ show a ++ ")"

instance Show a => Show (Prog a) where
   show (Seq p q)      = "(" ++ show p ++ " `Seq` " ++ show q ++ ")"
   show (Nondet [])    = "Nondet []"
   show (Nondet ps)    = "Nondet [" ++ foldl1 (\s1 s2 -> s1 ++ ", " ++ s2) (map show ps) ++ "]"
   show (Conc p q)     = "(" ++ show p ++ " `Conc` " ++ show q ++ ")"
   show (PseudoAtom p) = show p
   show Nil            = "Nil"

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

