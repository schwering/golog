{-# LANGUAGE TemplateHaskell #-}
module Util.InterpolationTest where

import Util.ExtFrac
import Util.Interpolation

import System.Random
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers

type ExtRational = ExtFrac Rational

instance Arbitrary a => Arbitrary (ExtFrac a) where
   arbitrary = do
      n <- choose (0, 9) :: Gen Int
      x <- arbitrary :: Gen a
      return $ case n of
                    0 -> NegInf
                    1 -> PosInf
                    2 -> NaN
                    _ -> error ""

prop_interpol_lin :: ExtRational -> ExtRational -> ExtRational -> Bool
prop_interpol_lin m c y = f (interpolateLin id (-1, 1) y f) == y
   where f x = m * x + c

runTests :: IO Bool
runTests = $quickCheckAll

