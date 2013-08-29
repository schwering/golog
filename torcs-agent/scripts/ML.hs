{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ML where

import AI.HNN.FF.Network
import Data.Either (rights)
import Numeric.LinearAlgebra
import System.IO
import System.IO.Unsafe
import Text.ParserCombinators.Parsec

newtype KmH = KmH { kmh :: Double }
   deriving (Num, Floating, Fractional, Real, RealFloat, RealFrac, Eq, Ord)

newtype Rad = Rad { rad :: Double }
   deriving (Num, Floating, Fractional, Real, RealFloat, RealFrac, Eq, Ord)

newtype Force = Force { force :: Double }
   deriving (Num, Floating, Fractional, Real, RealFloat, RealFrac, Eq, Ord)

newtype Sec = Sec { sec :: Double }
   deriving (Num, Floating, Fractional, Real, RealFloat, RealFrac, Eq, Ord)

instance Read KmH where
   readsPrec i = map (\(x,s') -> (KmH x,s')) . readsPrec i

instance Show KmH where
   showsPrec i (KmH d) = showsPrec i d
   show (KmH d) = show d

instance Read Rad where
   readsPrec i = map (\(x,s') -> (Rad x,s')) . readsPrec i

instance Show Rad where
   showsPrec i (Rad d) = showsPrec i d
   show (Rad d) = show d

instance Read Force where
   readsPrec i = map (\(x,s') -> (Force x,s')) . readsPrec i

instance Show Force where
   showsPrec i (Force d) = showsPrec i d
   show (Force d) = show d

instance Read Sec where
   readsPrec i = map (\(x,s') -> (Sec x,s')) . readsPrec i

instance Show Sec where
   showsPrec i (Sec d) = showsPrec i d
   show (Sec d) = show d

data Example = Example { speedX0  :: KmH, speedX1 :: KmH
                       , speedY0  :: KmH, speedY1 :: KmH
                       , angle0   :: Rad, angle1  :: Rad
                       , accel0   :: Force
                       , steer0   :: Force
                       , timeStep :: Sec
                       } deriving Show

deg2rad :: Double -> Rad
deg2rad deg = Rad (deg * 0.017453292519943295)

rad2deg :: Rad -> Double
rad2deg (Rad rad) = rad / 0.017453292519943295

kmh2ms :: KmH -> Double
kmh2ms (KmH kmh) = kmh / 3.6

ms2kmh :: Double -> KmH
ms2kmh ms = KmH (ms * 3.6)

examplesIO :: String -> IO [Example]
examplesIO fn = readFile fn >>= return . map make . rights . map parseLine . lines
   where make :: [Double] -> Example
         make xs = Example { speedX0  = KmH $ xs !! 0, speedX1 = KmH $ xs !! 1
                           , speedY0  = KmH $ xs !! 2, speedY1 = KmH $ xs !! 3
                           , angle0   = Rad $ xs !! 4, angle1  = Rad $ xs !! 5
                           , accel0   = Force $ (xs !! 6) - (xs !! 7)
                           , steer0   = Force $ xs !! 8
                           , timeStep = Sec $ xs !! 9
                           }

         parseLine :: String -> Either ParseError [Double]
         parseLine input = parse lineFmt source input
            where source = "(string: " ++ input ++ ")"

         lineFmt :: GenParser Char st [Double]
         lineFmt = many number

         number :: GenParser Char st Double
         number = do _ <- many space
                     x <- many1 (noneOf "() \n")
                     _ <- many space
                     return (read x)

examples :: [Example]
examples = unsafePerformIO $ examplesIO "bla2"

example2sample :: Example -> Sample Double
example2sample ex = (fromList [{-kmh2ms (speedX0 ex),-} force (accel0 ex)], fromList [kmh2ms (speedX1 ex - speedX0 ex)])

samples :: Samples Double
samples = map example2sample examples

test :: IO ()
test = do   n <- createNetwork 1 [2] 1
            --mapM_ (putStrLn . show . output n tanh . fst) samples
            putStrLn "------------------"
            let n' = trainNTimes 100000 0.8 tanh tanh' n samples
            --mapM_ (putStrLn . show . present n) samples
            putStrLn ("Average error: " ++ show (avgErr n samples))
   where present n (inp, outp) = (toList inp, head $ toList obs, head $ toList outp, head $ diff)
            where obs  = output n tanh inp
                  diff = map (uncurry (-)) (zip (toList obs) (toList outp))
         avgErr n samples = sum (map err samples) / fromIntegral (length samples)
            where err (inp, outp) = abs ((head $ toList $ output n tanh inp) - (head $ toList $ outp)) / abs (head $ toList $ output n tanh inp)

