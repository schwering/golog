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
   arbitrary = do
      n <- choose (0, 9) :: Gen Int
      x <- arbitrary :: a
      return $ case n of
                    0 -> NegInf
                    1 -> PosInf
                    2 -> NaN
                    _ -> Val x

prop_mult :: Rational -> Rational -> Bool
prop_mult NaN _ = NaN
prop_mult _ NaN = NaN
prop_mult (Val x) (Val y) | x /= 0 && y /= 0 = Val (x / y)

runTests :: IO Bool
runTests = $quickCheckAll

