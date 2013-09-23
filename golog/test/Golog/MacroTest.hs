{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Golog.MacroTest where

import Data.List (inits)
import Golog.Interpreter
import Golog.Macro
import Golog.Util
import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Debug.Trace

data A = A | B | C | D | Test (Sit A -> Bool)

instance Show A where
   show A        = "A"
   show B        = "B"
   show C        = "C"
   show D        = "D"
   show (Test _) = "Test"

instance Eq A where
   A      == A      = True
   B      == B      = True
   C      == C      = True
   D      == D      = True
   Test _ == Test _ = True
   --Test _ == _      = error "Eq.(==): Test"
   --_      == Test _ = error "Eq.(==): Test"
   _      == _      = False

instance TestAction A where
   testAction = Test

instance BAT A where
   data Sit A = Sit [A] deriving (Show, Eq)

   s0  = Sit []

   --do_ (Test _) s        = s
   do_ a        (Sit as) = Sit (as ++ [a])

   poss (Test f) s = f s
   poss _        _ = True


prop_Star = take 10 (map sit (doo (treeND p s0))) ==
            take 10 (map Sit (inits (repeat A)))
   where p = iter $ prim A

prop_ifThenElse1 = map sit (doo (treeND p s0)) == [Sit [t,D,t]]
   where p = if_ (\(Sit as) -> D `notElem` as)
               (then_ $ test (\(Sit as) -> D `elem` as))
               (else_ $ test (\_        -> False))
             `Conc` prim D
         t = Test undefined

prop_ifThenElseA1 = map sit (doo (treeND p s0)) == []
   where p = ifA (\(Sit as) -> D `notElem` as)
               (then_ $ test (\(Sit as) -> D `elem` as))
               (else_ $ test (\_        -> False))
             `Conc` prim D

prop_ifThenElseA2 = map sit (doo (treeND p s0)) == [Sit [t,A,C,B]
                                                   ,Sit [t,A,B,C]
                                                   ,Sit [C,t,A,B]]
   where p = ifA (\_ -> True)
               (then_ $ prim A `Seq` prim B)
               (else_ $ prim A `Seq` prim B)
             `Conc` prim C
         t = Test undefined

prop_ifThenElseA3 = map sit (doo (treeND p s0)) == [Sit [A,C,t,B]
                                                   ,Sit [A,t,C,B]
                                                   ,Sit [A,t,B,C]
                                                   ,Sit [C,A,t,B]]
   where p = ( prim A `Seq`
               ifA (\_ -> True)
                 (then_ $ Nil)
                 (else_ $ Nil) `Seq`
               prim B
             ) `Conc` prim C
         t = Test undefined

prop_while1 = map sit (doo (treeND p s0)) == [Sit [t,A,t,C], Sit [t,B,t,C]]
   where p = while (\(Sit as) -> A `notElem` as && B `notElem` as)
                   (choice $ map prim [A,B])
             `Seq` prim C
         t = Test undefined

prop_while2 = fmap sit (doo' (treeND p s0)) == Just (Sit [t,A,t,B,t,C,t,D])
   where p = while (\(Sit as) -> filter (not.isTest) as /= [A,B,C])
                   (choice $ map prim [A,B,C])
             `Seq` prim D
         t = Test undefined
         isTest (Test _) = True
         isTest _        = False

prop_while3 = map sit (doo (treeND p s0)) == [Sit [B,t]
                                             ,Sit [t,B,A,t]
                                             ,Sit [t,A,B,t]
                                             ,Sit [t,A,t,B]]
   where p = while (\(Sit as) -> A `notElem` as && B `notElem` as)
                   (prim A)
             `Conc` prim B
         t = Test undefined
         isTest (Test _) = True
         isTest _        = False

prop_whileA3 = map sit (doo (treeND p s0)) == [Sit [B,t]
                                              ,Sit [t,A,B,t]
                                              ,Sit [t,A,t,B]]
   where p = whileA (\(Sit as) -> A `notElem` as && B `notElem` as)
                   (prim A)
             `Conc` prim B
         t = Test undefined
         isTest (Test _) = True
         isTest _        = False

runTests :: IO Bool
runTests = $quickCheckAll

