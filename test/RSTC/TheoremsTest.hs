{-# LANGUAGE TemplateHaskell #-}
module RSTC.TheoremsTest where

import Car
import RSTC.Theorems
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Control.Monad (liftM2)
import Data.List

data Info a = Info { v :: a,
                     x :: a } deriving (Eq, Show)

type Map a = [(Car, Info a)]


instance Arbitrary Car where
   arbitrary = oneof (map return cars)

instance (Arbitrary a) => Arbitrary (Info a) where
   arbitrary = liftM2 (\v x -> Info v x) arbitrary arbitrary
   shrink (Info v x) = [ Info v' x | v' <- shrink v ]
                    ++ [ Info v x' | x' <- shrink x ]


(!) :: Map a -> Car -> Maybe (Info a)
[]          ! b             = Nothing
((c, cc):m) ! b | b == c    = Just cc
                | otherwise = m ! b

ntg m b c
   | b == c    = 0/0
   | otherwise = let mb = m ! b
                     mc = m ! c
                 in case (mb, mc) of
                     (Just bb, Just cc) -> ((x cc) - (x bb)) / (v bb)
                     (_,       _)       -> 0/0

ttc m b c
   | b == c    = 0/0
   | otherwise = let mb = m ! b
                     mc = m ! c
                 in case (mb, mc) of
                     (Just bb, Just cc) -> ((x cc) - (x bb)) / ((v bb) - (v cc))
                     (_,       _)       -> 0/0



sim :: Double -> Double -> Bool
sim x y = abs (x - y) < 0.00001


ssim :: Double -> Double -> Bool
ssim x y = nok x || sim x y


ok x = not (isNaN x) && not (isInfinite x)
nok = not . ok


present m c = case m ! c of Nothing -> True
                            Just _  -> False


prop_def_if :: NonEmptyList (Car, Info Double) -> Car -> Car -> Bool
prop_def_if (NonEmpty m) b c = if (nok (ttc m b c)) then (nok (ntg m b c)) else True


prop_def_oif :: NonEmptyList (Car, Info Double) -> Car -> Car -> Bool
prop_def_oif (NonEmpty m) b c = if (nok (ntg m b c)) then (nok (ttc m b c)) else True


prop_ntgSymm :: NonEmptyList (Car, Info Double) -> Bool
prop_ntgSymm (NonEmpty m) = and [ (ntg m b c) `ssim` ntgSymm (ntg m) (ttc m) c b | b <- cars, c <- cars ]


prop_ttcSymm :: NonEmptyList (Car, Info Double) -> Bool
prop_ttcSymm (NonEmpty m) = and [ (ttc m b c) `ssim` ttcSymm (ntg m) (ttc m) c b | b <- cars, c <- cars ]


-- XXX Why is the stupid 'present' needed?!
prop_ntgTrans :: NonEmptyList (Car, Info Double) -> Bool
prop_ntgTrans (NonEmpty m) = and [ (ntg m b c) `ssim` ntgTrans (ntg m) (ttc m) b c d | b <- cars, c <- cars, d <- cars, b /= c, b /= d, c /= d, present m b, present m c, present m d ]


-- XXX Why is the stupid 'present' needed?!
prop_ttcTrans :: NonEmptyList (Car, Info Double) -> Bool
prop_ttcTrans (NonEmpty m) = and [ (ttc m b c) `ssim` ttcTrans (ntg m) (ttc m) b c d | b <- cars, c <- cars, d <- cars, b /= c, b /= d, c /= d, present m b, present m c, present m d ]


runTests :: IO Bool
runTests = $quickCheckAll

