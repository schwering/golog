{-# LANGUAGE TemplateHaskell #-}
module Util.NativePSOTest where

import System.Random
import Test.QuickCheck.All
import Util.NativePSO

prop_max_sq :: Int -> Bool
prop_max_sq seed = let r = pso seed 50 5 defaultParams (-5, 5) (Max (\x -> -x*x))
                   in -0.1 < r && r < 0.1

prop_max_sq2 :: Bool
prop_max_sq2 = let r = pso 10 100 5 defaultParams (-5, 5) (Max (\x -> -x*x))
               in -0.01 < r && r < 0.01

prop_min_sq :: Int -> Bool
prop_min_sq seed = let r = pso seed 50 5 defaultParams (-5, 5) (Min (\x -> x*x))
                   in -0.1 < r && r < 0.1

prop_min_abs :: Int -> Bool
prop_min_abs seed = let r = pso seed 50 5 defaultParams (-2, 2) (Min abs)
                    in -0.1 < r && r < 0.1

prop_min_abs2 :: Bool
prop_min_abs2 = let r = pso 10 50 5 defaultParams (-100, 100) (Min abs)
                in -0.1 < r && r < 0.1

prop_max_poly :: Bool
prop_max_poly = let r = pso 10 1000 5 defaultParams (-1000, 1000) (Min (\x -> x*x*x*x - 5.0 * x*x + 4.0))
                    opts   = [-1.58113883, 1.58113883]
                    eps    = 0.05
                    diffs  = map (\opt -> opt - r) opts
                in any (\diff -> -eps < diff && diff < eps) diffs

runTests :: IO Bool
runTests = $quickCheckAll

