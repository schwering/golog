{-# LANGUAGE TemplateHaskell #-}
module Util.PSOTest where

import System.Random
-- import System.IO.Unsafe (unsafePerformIO) -- Print results with: r' = unsafePerformIO (putStrLn (show r) >>= \_ -> return r)
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Util.PSO

run_pso :: Int -> IterationCount -> ParticleCount -> Bounds Double -> Objective Double Double -> Double
run_pso seed m n (lo, hi) obj = let Vector1d result = opt
                                in result
   where params  = defaultParams
         bounds  = (Vector1d lo, Vector1d hi)
         obj'    = case obj of Max f -> Max (\(Vector1d x) -> f x)
                               Min f -> Min (\(Vector1d x) -> f x)
         gen     = mkStdGen seed
         (opt,_) = pso m n params bounds obj' gen


prop_max_sq :: Int -> Bool
prop_max_sq seed = let result = run_pso seed 5 50 (-5, 5) (Max (\x -> -x*x))
                   in -0.1 < result && result < 0.1

prop_max_sq2 :: Bool
prop_max_sq2 = let result = run_pso 10 5 100 (-5, 5) (Max (\x -> -x*x))
               in -0.01 < result && result < 0.01

prop_min_sq :: Int -> Bool
prop_min_sq seed = let result = run_pso seed 5 50 (-5, 5) (Min (\x -> x*x))
                   in -0.1 < result && result < 0.1

prop_min_abs :: Int -> Bool
prop_min_abs seed = let result = run_pso seed 5 50 (-2, 2) (Min abs)
                    in -0.1 < result && result < 0.1

prop_min_abs2 :: Bool
prop_min_abs2 = let result = run_pso 10 5 50 (-100, 100) (Min abs)
                in -0.1 < result && result < 0.1

prop_max_poly :: Bool
prop_max_poly = let result = run_pso 10 5 1000 (-1000, 1000) (Min (\x -> x*x*x*x - 5.0 * x*x + 4.0))
                    opts   = [-1.58113883, 1.58113883]
                    eps    = 0.05
                    diffs  = map (\opt -> opt - result) opts
                in any (\diff -> -eps < diff && diff < eps) diffs

runTests :: IO Bool
runTests = $quickCheckAll

