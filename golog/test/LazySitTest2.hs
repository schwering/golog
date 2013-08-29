{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | A simple BAT for manual comparison between the old and new Golog
-- interpreters.
module LazySitTest2 where

import Data.Maybe (fromJust)
import Golog.Interpreter
import Golog.Macro
import Golog.Util hiding (HistBAT(..))

instance BAT Int where
   data Sit Int = S0 | Do Int (Sit Int) deriving Show
   s0 = S0
   do_ a (Sit as) = Sit (as ++ [a])
   poss a _ = even a

-- The tilde is important to avoid early pattern matching of the second
-- parameter which represents the future.
then_ :: Sit Int -> Sit Int -> Sit Int
then_ s ~(Sit as') = Sit (as ++ as')
--then_ (Sit as) a h = Sit (as ++ [a] ++ (case h of Sit as' -> as' ; _ -> []))

instance DTBAT Int where
   reward _ s        | sitlen s > 5                  = -1000
   reward a (Sit as) | length as > 0 && last as == a = -1
   reward a _                                        = fromIntegral (max 0 a)

instance Monad m => IOBAT Int m where
   syncA 4 s = return $ do_ 2 s
   syncA a s = return $ do_ a s

sitlen :: Sit Int -> Int
sitlen (Sit as) = length as

testLazy = take 10 as
   where Sit as = fake4
         fake0 = Sit (fake1' p)
         fake0' (PseudoAtom (Atom (Prim a)) `Seq` p) = a : fake0' p

         fake1 = Sit (fake1' p)
         fake1' (PseudoAtom (Atom (Prim a)) `Seq` p) = cons a (fake1' p)

         fake2 = fake2' (Sit []) p
         fake2' (Sit as) (PseudoAtom (Atom (Prim a)) `Seq` p) = fake2' (Sit (as ++ [a])) p

         fake3 = fake3' s0 p
         fake3' s (PseudoAtom (Atom (Prim a)) `Seq` p) = fake3' (do_ a s) p

         fake4 = fake4' (s0 :: Sit Int) p
         fake4' s (PseudoAtom (Atom (Prim a)) `Seq` p) = (do_ a s) `then_` (fake4' s p)

         s = sit $ fromJust $ doo' $ treeND p s0
         p = p' 0 :: Prog Int
         p' n = prim n `Seq` p' (n+2)
         cons = (:)

