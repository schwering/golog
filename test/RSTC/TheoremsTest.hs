{-# LANGUAGE TemplateHaskell #-}
module RSTC.TheoremsTest where

import Prelude hiding ((!!))
import RSTC.Car
import RSTC.Theorems
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Control.Monad (liftM2)
import Data.List hiding ((!!))

data Info a = Info { v :: a,
                     x :: a } deriving (Eq, Show)

type Map a = [(Car, Info a)]


instance Arbitrary Car where
   arbitrary = oneof (map return cars)

instance Arbitrary a => Arbitrary (Info a) where
   arbitrary = liftM2 (\v x -> Info v x) arbitrary arbitrary
   shrink (Info v x) = [ Info v' x | v' <- shrink v ]
                    ++ [ Info v x' | x' <- shrink x ]


(!) :: Map a -> Car -> Maybe (Info a)
[]          ! b             = Nothing
((c, cc):m) ! b | b == c    = Just cc
                | otherwise = m ! b

(!!) :: Map a -> Car -> Info a
[]          !! b             = error ("RSTC.TheoremsTest.!!: [] " ++ (show b))
((c, cc):m) !! b | b == c    = cc
                 | otherwise = m !! b

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


present m c = case m ! c of Nothing -> False
                            Just _  -> True


-- prop_* :: NonEmptyList (Car, Info Double) -> [Car -> Car ->] Bool

prop_def_if (NonEmpty m) b c = if (nok (ttc m b c)) then (nok (ntg m b c)) else True
prop_def_oif (NonEmpty m) b c = if (nok (ntg m b c)) then (nok (ttc m b c)) else True


prop_ntgSymm (NonEmpty m) = and [ (ntg m b c) `ssim` ntgSymm (ntg m) (ttc m) c b | b <- cars, c <- cars ]
prop_ttcSymm (NonEmpty m) = and [ (ttc m b c) `ssim` ttcSymm (ntg m) (ttc m) c b | b <- cars, c <- cars ]
prop_ntgTrans (NonEmpty m) = and [ (ntg m b d) `ssim` ntgTrans (ntg m) (ttc m) b c d | b <- cars, c <- cars, d <- cars, b /= c, b /= d, c /= d, present m c ]
prop_ttcTrans (NonEmpty m) = and [ (ttc m b d) `ssim` ttcTrans (ntg m) (ttc m) b c d | b <- cars, c <- cars, d <- cars, b /= c, b /= d, c /= d, present m c ]


prop_relVeloc :: NonEmptyList (Car, Info Double) -> Bool
prop_relVeloc (NonEmpty m) = and [ relVeloc (ntg m) (ttc m) b c `ssim` (v (m !! b) / v (m !! c)) | b <- cars, c <- cars, b /= c, present m b, present m c ]


prop_following :: NonEmptyList (Car, Info Double) -> Bool
prop_following (NonEmpty m) = and [ not (x (m !! b) < x (m !! c) && v (m !! b) > 0 && v (m !! c) > 0) || isFollowing (ntg m) b c | b <- cars, c <- cars, b /= c, present m b, present m c ]

prop_followed :: NonEmptyList (Car, Info Double) -> Bool
prop_followed (NonEmpty m) = and [ not (x (m !! b) > x (m !! c) && v (m !! b) > 0 && v (m !! c) > 0) || isFollowed (ntg m) b c | b <- cars, c <- cars, b /= c, present m b, present m c ]

prop_approaching :: NonEmptyList (Car, Info Double) -> Bool
prop_approaching (NonEmpty m) = and [ not (x (m !! b) < x (m !! c) && v (m !! b) > 0 && v (m !! c) < 0) || isApproaching (ntg m) b c | b <- cars, c <- cars, b /= c, present m b, present m c ]

prop_aparting :: NonEmptyList (Car, Info Double) -> Bool
prop_aparting (NonEmpty m) = and [ not (x (m !! b) < x (m !! c) && v (m !! b) < 0 && v (m !! c) > 0) || isMovingApart (ntg m) b c | b <- cars, c <- cars, b /= c, present m b, present m c ]

prop_converging :: NonEmptyList (Car, Info Double) -> Bool
prop_converging (NonEmpty m) = and [ not (x (m !! b) < x (m !! c) && v (m !! b) > v (m !! c)) || isConverging (ttc m) b c | b <- cars, c <- cars, b /= c, present m b, present m c ]

prop_diverging :: NonEmptyList (Car, Info Double) -> Bool
prop_diverging (NonEmpty m) = and [ not (x (m !! b) < x (m !! c) && v (m !! b) < v (m !! c)) || isDiverging (ttc m) b c | b <- cars, c <- cars, b /= c, present m b, present m c ]


runTests :: IO Bool
runTests = $quickCheckAll

