{-# LANGUAGE TemplateHaskell #-}
module Util.InterpolationTest where

--import Util.ExtFrac
import Util.Interpolation

import Control.Monad
import Data.List (sortBy)
import System.Random
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers

{-
type ExtRational = ExtFrac Rational

instance Arbitrary a => Arbitrary (ExtFrac a) where
   arbitrary = frequency [(1, return NaN), (3, return NegInf), (3, return PosInf), (9, liftM Val arbitrary)]
   shrink (Val x) = NaN : NegInf : PosInf : [ Val x' | x' <- shrink x ]
   shrink _       = []
-}

--prop_interpol_lin :: ExtRational -> ExtRational -> ExtRational -> Bool
prop_interpol_lin :: Double -> Double -> Double -> Bool
prop_interpol_lin m c y = all (\x -> f x `similar` y) (nullAt id (canonicalize Linear f y))
   where f x = m * x + c

prop_interpol_recip :: Double -> Double -> Double -> Double -> Bool
prop_interpol_recip c1 c2 c3 y | abs c1 > 1000 || abs c2 > 1000 || abs c3 > 1000 = True
prop_interpol_recip c1 c2 c3 y = all (\x -> f x `similar` y) (nullAt id (canonicalize Recip f y))
   where f x = 1 / (c1*x + c2) + c3

prop_interpol_lin_sum :: Double -> Double -> Double -> Double -> Double -> Bool
prop_interpol_lin_sum c1 c2 d1 d2 y | abs c1 > 1000 || abs c2 > 1000 ||
                                      abs d1 > 1000 || abs d2 > 1000 = True
prop_interpol_lin_sum c1 c2 d1 d2 y = all (\x -> (f x + g x) `similar` y) (nullAt id (canonicalizeSum Linear f g y))
   where f x = c1*x + c2
         g x = d1*x + d2

prop_interpol_recip_sum :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Bool
prop_interpol_recip_sum c1 c2 c3 d1 d2 d3 y | abs c1 > 1000 || abs c2 > 1000 || abs c3 > 1000 ||
                                              abs d1 > 1000 || abs d2 > 1000 || abs d3 > 1000 = True
prop_interpol_recip_sum c1 c2 c3 d1 d2 d3 y = all (\x -> (f x + g x) `similar` y) (nullAt id (canonicalizeSum Recip f g y))
   where f x = 1 / (c1*x + c2) + c3
         g x = 1 / (d1*x + d2) + d3

prop_interpol_recip_normal_eq_sum :: Double -> Double -> Double -> Bool
prop_interpol_recip_normal_eq_sum c1 c2 c3 | c1 == 0 || c2 == 0 || c3 == 0 = True
--prop_interpol_recip_normal_eq_sum c1 c2 c3 = y1 `similar` y2
prop_interpol_recip_normal_eq_sum c1 c2 c3 = x1 `similar` x2
   where f x = 1 / (c1*x + c2) + c3
         y   = 0
         xs1 = nullAt id (canonicalize Recip f y)
         xs2 = nullAt id (canonicalizeSum Recip f f y)
         x1  = head $ xs1
         x2  = pick $ xs2
         y1  = f x1
         y2  = f x2 + f x2
         pick xs = case sortBy (\x y -> compare (abs (f x + f x)) (abs (f y + f y))) xs of (x:_) -> x ; [] -> 0/0

similar :: (Ord a, Fractional a) => a -> a -> Bool
similar x y | y == 0    = x == 0
            | otherwise = ( signum x == signum y &&
                            abs x / abs y >= 0.99 &&
                            abs x / abs y <= 1.01 ) ||
                          abs (x - y) < 1/(1000*1000*1000)

similarList :: (Ord a, Fractional a) => [a] -> [a] -> Bool
similarList xs ys = undefined

runTests :: IO Bool
runTests = $quickCheckAll

