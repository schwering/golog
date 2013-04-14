{-# LANGUAGE TemplateHaskell #-}
module Util.ExtFracTest where

import Util.ExtFrac

import System.Random
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Control.Monad (liftM)

instance Arbitrary a => Arbitrary (ExtFrac a) where
   arbitrary = frequency [(1, return NaN), (3, return NegInf), (3, return PosInf), (9, liftM Val arbitrary)]
   shrink (Val x) = NaN : NegInf : PosInf : [ Val x' | x' <- shrink x ]
   shrink _       = []

{-
prop_mult :: ExtFrac Double -> ExtFrac Double -> Bool
prop_mult x @ NaN       y                               = x * y == NaN
prop_mult y             x @ NaN                         = x * y == NaN
prop_mult x @ (Val x')  y @ (Val y') | x' /= 0 && y' /= 0 = x * y == Val (x' * y')
                                     | x' > 0  && y' == 0 = x * y == PosInf
                                     | x' < 0  && y' == 0 = x * y == NegInf
                                     | x' == 0 && y' == 0 = x * y == NaN
-}

runTests :: IO Bool
runTests = $quickCheckAll

