{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Golog.InterpreterTest where

import Data.List (sort)
import Data.Maybe (fromJust)
import Golog.Interpreter
import Golog.Macro
import Golog.Util
import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Debug.Trace

newtype Lookahead = Lookahead Depth deriving Show

instance Arbitrary Lookahead where
   arbitrary = fmap Lookahead $ oneof $ map return [0..9]

instance BAT Int where
   data Sit Int = S0 | Do Int (Sit Int) deriving (Eq, Show, Ord)
   s0  = S0
   do_ = Do
   poss _ S0       = True
   poss i (Do j s) = even i == odd j

instance DTBAT Int where
   reward i _ = realToFrac i

instance Monad m => IOBAT Int m where
   syncA a s = return $ do_ a s

trans'' = head . trans

p :: Int -> Prog Int
p = prim

prop_final1 = final $ treeND (Nil :: Prog Int) s0
prop_final2 = not $ final $ treeND (p 1 `Seq` p 2) s0
prop_final3 = final $ treeND (Nondet [Nil, p 1 `Seq` p 2]) s0
prop_final4 = final $ treeND (star (p 1 `Seq` p 2)) s0
prop_final5 = not $ final $ treeND (plus (p 1 `Seq` p 2)) s0
prop_final6 = not $ final $ trans'' $ treeND (p 1 `Seq` p 2) s0
prop_final7 = final $ trans'' $ trans'' $ treeND (p 1 `Seq` p 2) s0
prop_final8 = not $ final $ trans'' $ treeND (star (p 1 `Seq` p 2)) s0
prop_final9 = final $ trans'' $ trans'' $ treeND (star (p 1 `Seq` p 2)) s0

prop_final10 (Lookahead l) = not $ final $                     treeDT l (star (p 1 `Seq` p 2)) s0
prop_final11 (Lookahead l) = not $ final $ trans'' $           treeDT l (star (p 1 `Seq` p 2)) s0
prop_final12 (Lookahead l) = not $ final $ trans'' $ trans'' $ treeDT l (star (p 1 `Seq` p 2)) s0
prop_final19 (Lookahead l) =       final $                     treeDT l (star (p (-1) `Seq` p (-2))) s0
prop_final23 (Lookahead l) =       final $ trans'' $           treeDT l (p 0 `Seq` star (p (-1) `Seq` p (-2))) s0
prop_final24 (Lookahead l) = (l /= 0 && l /= 2) == (final $ treeDT l (star (p 1 `Seq` p (-2))) s0)

prop_final25 (Lookahead l) = not $ final $ treeDT l (p 1 `Seq` p 2) s0

prop_doo1 = map sit (doo (treeND (p 1 `Seq` Nondet [p i `Seq` p (i+1) | i <- [1..5]]) s0)) ==
            map list2sit [[1,2,3], [1,4,5]]
prop_doo2 = sort (map sit (doo (treeND (p 1 `Seq` ((p 10 `Seq` (atomic $ p 11 `Seq` p 12) `Seq` p 13) `Conc` (p 20 `Seq` p 21)) `Seq` p 100) s0))) ==
            sort (map list2sit [[1,10,11,12,13,20,21,100], [1,20,21,10,11,12,13,100]])
prop_doo3 = take 11 (map sit (doo (treeND (p 0 `Seq` (star (primf (\(Do i _) -> i+1)))) s0))) ==
            [list2sit [0..i] | i <- [0..10]]

prop_doo4 (Lookahead l) =
            map sit (doo (treeDT l (p 1 `Seq` Nondet [p i `Seq` p (i+1) | i <- [1..5]]) s0)) ==
            map list2sit [[1,4,5]]

prop_sync = let t = treeNDIO (p 1 `Seq` Nondet [p i `Seq` p (i+1) | i <- [1..5]]) s0
            in fmap sit (doo' t >>= sync) == fmap sit (fromJust (dooSync' t))

runTests :: IO Bool
runTests = $quickCheckAll

