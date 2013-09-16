{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Golog.Util
   (HistBAT(..), defaultPredSit, sit2list, list2sit, append2sit,
    trans', transStarBFS, transStarDFS, transStarDFS',
    doo, doo',
    Mode(..), Search(..), dooIO) where

import Control.Monad ((>=>))
import Data.List (foldl')
import Data.Maybe (listToMaybe)
import qualified Data.Foldable as F
import qualified Data.Tree as T
import Golog.Interpreter

-- Class for situations which store a history.
-- Minimal implementation is 'history' or 'since'.
class BAT a => HistBAT a where
   -- | The number of actions executed in the situation since s0.
   sitlen  :: Sit a -> Int

   -- | The actions executed in the situation since s0, ordered latest action
   -- first (i.e., in reverse chronological order).
   history :: Sit a -> [a]

   -- | All actions executed in the first situation since the second situation.
   -- E.g., since (Do a s) s = [a] should hold.
   -- The implementation may assume that the second situation is indeed
   -- predecessor situation of the first one or the same. This assumption avoids
   -- expensive checks for equality.
   since   :: Sit a -> Sit a -> [a]

   -- | The last action and the predecessor situation.
   -- The default implementation is quite expensive as it rebuilds the whole
   -- situation by applying 'do_'.
   -- While regressive BATs may have trivial implementations, progressive BATs
   -- may store the previous situation explicitly to avoid this rebuilding.
   predSit :: Sit a -> Maybe (a, Sit a)

   sitlen      = length . history
   history s   = since s s0
   since s2 s1 = take (sitlen s2 - sitlen s1) (history s2)
   predSit     = defaultPredSit

defaultPredSit :: HistBAT a => Sit a -> Maybe (a, Sit a)
defaultPredSit s   = case history s of []   -> Nothing
                                       a:as -> Just (a, foldr do_ s0 as)

sit2list :: HistBAT a => Sit a -> [a]
sit2list = reverse . history

list2sit :: BAT a => [a] -> Sit a
list2sit = append2sit s0

-- | Appends list of actions in given order to situation term as new actions.
append2sit :: BAT a => Sit a -> [a] -> Sit a
append2sit = foldl' (flip do_)

-- | Search mode used in during 'Offline' execution in 'dooIO'.
data Search = BFS | DFS deriving Eq

-- | Execution mode in 'dooIO'.
data Mode = Online | Offline Search deriving Eq

--- | Variant of 'trans' which commits to the first option.
trans' :: Conf a b -> Maybe (Conf a b)
trans' = listToMaybe . trans

-- | Returns a list of lists of configurations where the @n@-th list of
-- configuration is the result of applying 'trans' to the configurations in the
-- @n-1@-th list.
transStarBFS :: Conf a b -> [[Conf a b]]
transStarBFS c = takeWhile (not.null) $ iterate (concatMap trans) [c]

-- Returns a tree of configurations where each branch indicates the various
-- opportunities in the respective configuration.
-- We use a tree instead of lists of lists because the latter for reasons which
-- I don't understand has some issues with lazy evaluation.
transStarDFS :: Conf a b -> T.Tree (Conf a b)
transStarDFS c = T.Node c (map transStarDFS (trans c))

-- | Variant of 'transStarDFS' which commits to the first option in each
-- transition.
transStarDFS' :: Conf a b -> [Conf a b]
transStarDFS' c = c : maybe [] transStarDFS' (trans' c)

-- | Returns the list of final configurations reached from the given one.
doo :: Conf a b -> [Conf a b]
doo c = concat $ map (filter final) (transStarBFS c)

-- | Variant of 'doo' which commits to the first option in each configuration.
doo' :: Conf a b -> Maybe (Conf a b)
doo' = listToMaybe . doo

-- | Execution in online and/or offline mode.
-- The first parameter may assign to a configuration whether at this point
-- 'dooIO' should continue with 'Online' execution or 'Offline' execution,
-- where the latter distinguishes between 'BFS' and 'DFS'.
--
-- The latest point the current configuration is 'sync'ed is when it is 'final'
-- and execution therefore ends.
-- This is also the case for 'Offline' execution, i.e., 'dooIO' implicitly
-- switches to 'Online' mode at the end.
--
-- Pre-'final' 'sync's happen when for each configuration which is the successor
-- of a configuration which is assigned mode 'Online'.
--
-- Thus, if all configurations are assigned mode 'Online', 'sync' is called
-- after each transition.
-- On the other hand, if some configuration with 'Offline' mode and search
-- strategy 'DFS' is encountered, 'dooIO' starts a depth-first search which ends
-- successfully when it hits a 'final' configuration or a configuration with a
-- different mode. In the former case, the configuration is 'sync'ed and
-- returned. In the latter case, execution continues with the new mode. If this
-- new mode is 'Online', this means a pre-'final' 'sync'hronization point. If it
-- is 'Offline', it continues with the new search strategy.
dooIO :: Monad m => (ConfIO a m -> Mode) -> ConfIO a m -> m (Maybe (ConfIO a m))
dooIO m c = case (m c,       final c) of
                 (Offline e, False) -> h (filter stop (transStar e c)) (dooIO m)
                 (Online,    False) -> h (trans c) (sync >=> dooIO m)
                 (_,         True)  -> sync c >>= return . Just
   where transStar BFS = concat . tail . transStarBFS
         transStar DFS = tail . F.foldr (:) [] . transStarDFS
         stop c'       = final c' || m c' /= m c
         h []    _     = return Nothing
         h (x:_) f     = f x

-- toLists :: Tree a -> [[a]]
-- toLists Empty     = [[]]
-- toLists (Alt ts)  = concat (map toLists ts)
-- toLists (Val x t) = [x:xs | xs <- toLists t]

--instance Show a => Show (Atom a) where
--   show (Prim a)  = "Prim " ++ show a
--   show (PrimF _) = "PrimF <...>"

instance (BAT a, Show a) => Show (Atom a) where
   show (Act a)     = show (a s0)
   show (Complex a) = "Complex (" ++ show a ++ ")"

instance (BAT a, Show a) => Show (Prog a) where
   show (Seq p q)    = "(" ++ show p ++ " `Seq` " ++ show q ++ ")"
   show (Nondet p q) = "(" ++ show p ++ " `Nondet` " ++ show q ++ ")"
   show (Conc p q)   = "(" ++ show p ++ " `Conc` " ++ show q ++ ")"
   show (Atom p)     = show p
   show Nil          = "Nil"

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
                                   ( if not (stop x) then showTree' (d-1) (n+1) t else "" )
         s n = replicate (2*n) ' '
-}

