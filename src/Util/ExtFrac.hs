module Util.ExtFrac where

import Data.Ratio

data ExtFrac a = NegInf | Val a | PosInf | NaN deriving Show


instance Eq a => Eq (ExtFrac a) where
   NegInf == NegInf = True
   Val x  == Val y  = x == y
   PosInf == PosInf = True
   _      == _      = False


instance (Num a, Ord a) => Num (ExtFrac a) where
   NaN    + _      = NaN
   _      + NaN    = NaN
   NegInf + PosInf = NaN
   PosInf + NegInf = NaN
   NegInf + _      = NegInf
   _      + NegInf = NegInf
   PosInf + _      = PosInf
   _      + PosInf = PosInf
   Val x  + Val y  = Val (x + y)

   NaN    - _      = NaN
   _      - NaN    = NaN
   NegInf - NegInf = NaN
   PosInf - PosInf = NaN
   NegInf - _      = NegInf
   _      - NegInf = PosInf
   PosInf - _      = PosInf
   _      - PosInf = NegInf
   Val x  - Val y  = Val (x - y)

   negate NaN      = NaN
   negate NegInf   = PosInf
   negate PosInf   = NegInf
   negate (Val x)  = Val (negate x)

   NaN    * _       = NaN
   _      * NaN     = NaN
   NegInf * NegInf  = PosInf
   NegInf * PosInf  = NegInf
   NegInf * (Val x) | x > 0     = NegInf
                    | x < 0     = PosInf
                    | otherwise = NaN
   (Val x) * NegInf | x > 0     = NegInf
                    | x < 0     = PosInf
                    | otherwise = NaN
   PosInf * NegInf  = NegInf
   PosInf * PosInf  = PosInf
   PosInf * (Val x) | x > 0     = PosInf
                    | x < 0     = NegInf
                    | otherwise = NaN
   (Val x) * PosInf | x > 0     = PosInf
                    | x < 0     = NegInf
                    | otherwise = NaN
   Val x  * Val y   = Val (x * y)

   abs NaN         = NaN
   abs NegInf      = PosInf
   abs PosInf      = PosInf
   abs (Val x)     = Val (abs x)

   signum NaN      = NaN
   signum NegInf   = Val (-1)
   signum PosInf   = Val 1
   signum (Val x)  = Val (signum x)

   fromInteger x   = Val (fromInteger x)


instance (Fractional a, Ord a) => Fractional (ExtFrac a) where
   NaN    / _                        = NaN
   _      / NaN                      = NaN
   NegInf / NegInf                   = NaN
   NegInf / PosInf                   = NaN
   NegInf / Val x | x > 0            = NegInf
                  | x < 0            = PosInf
                  | otherwise        = NaN
   PosInf / NegInf                   = NaN
   PosInf / PosInf                   = NaN
   PosInf / Val x | x > 0            = PosInf
                  | x < 0            = NegInf
                  | otherwise        = NaN
   Val x / NegInf | x > 0            = Val (-0)
                  | x < 0            = Val 0
                  | otherwise        = NaN
   Val x / PosInf | x > 0            = Val 0
                  | x < 0            = Val (-0)
                  | otherwise        = NaN
   Val x / Val y  | x > 0  && y == 0 = PosInf
                  | x < 0  && y == 0 = NegInf
                  | x == 0 && y == 0 = NaN
                  | otherwise        = Val (x / y)

   recip NaN                         = NaN
   recip NegInf                      = Val (-0)
   recip PosInf                      = Val 0
   recip (Val x) | x == 0            = PosInf
                 | otherwise         = Val (recip x)

   fromRational x | numerator x > 0  && denominator x == 0 = PosInf
                  | numerator x < 0  && denominator x == 0 = NegInf
                  | numerator x == 0 && denominator x == 0 = NaN
                  | otherwise                              = fromInteger (numerator x) / fromInteger (denominator x)

