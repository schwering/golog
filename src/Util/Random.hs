module Util.Random where

newtype Supply = Supply Int deriving Show

params :: (Int, Int, Int)
params = (9301, 49297, 233280)

init :: Int -> Supply
init = Supply

shuffle :: Int -> Supply -> Supply
shuffle k (Supply i) = Supply (i*k)

random :: Supply -> (Int, Supply)
random (Supply i) = let (a,c,m) = params
                        i' = ((i * a) + c) `mod` m
                    in (i', Supply i')

randomInRange :: Int -> Int -> Supply -> (Int, Supply)
randomInRange lo hi r = let (i',r') = random r
                            m       = randMax
                        in (lo + (hi - lo) * i' `div` m, r')

randCount :: Int
randCount = let (_,_,m) = params in m

randMax :: Int
randMax = let (_,_,m) = params in m - 1

