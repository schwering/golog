module Util.ExtFrac where

import Data.Ratio

data ExtFrac a = NegInf | Val a | PosInf | NaN deriving Show


instance Eq a => Eq (ExtFrac a) where
   NegInf == NegInf = True
   Val x  == Val y  = x == y
   PosInf == PosInf = True
   _      == _      = False

instance Ord a => Ord (ExtFrac a) where
   compare NaN        _ = error "ExtFrac.compare: NaN"
   compare _       NaN = error "ExtFrac.compare: NaN"
   compare NegInf  NegInf = EQ
   compare NegInf  _      = LT
   compare _       NegInf = GT
   compare PosInf  PosInf = EQ
   compare PosInf  _      = GT
   compare _       PosInf = LT
   compare (Val x) (Val y) = compare x y

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

instance (Floating a, Ord a) => Floating (ExtFrac a) where
   pi = Val pi
   exp NaN     = NaN
   exp NegInf  = Val 0
   exp PosInf  = PosInf
   exp (Val x) = Val (exp x)
   sqrt NaN     = NaN
   sqrt NegInf  = NegInf
   sqrt PosInf  = PosInf
   sqrt (Val x) | x >= 0    = Val (sqrt x)
                | otherwise = NaN
   log NaN     = NaN
   log NegInf  = NaN
   log PosInf  = PosInf
   log (Val x) | x > 0     = Val (log x)
               | x == 0    = NegInf
               | otherwise = NaN
   sin NaN     = NaN
   sin NegInf  = NaN
   sin PosInf  = NaN
   sin (Val x) = Val (sin x)
   cos NaN     = NaN
   cos NegInf  = NaN
   cos PosInf  = NaN
   cos (Val x) = Val (sin x)
   tan NaN     = NaN
   tan NegInf  = NaN
   tan PosInf  = NaN
   tan (Val x) = Val (tan x)
   asin NaN     = NaN
   asin NegInf  = NaN
   asin PosInf  = NaN
   asin (Val x) = Val (asin x)
   atan NaN     = NaN
   atan NegInf  = NaN
   atan PosInf  = NaN
   atan (Val x) = Val (atan x)
   acos NaN     = NaN
   acos NegInf  = NaN
   acos PosInf  = NaN
   acos (Val x) = Val (acos x)
   sinh NaN     = NaN
   sinh NegInf  = NegInf
   sinh PosInf  = PosInf
   sinh (Val x) = Val (sinh x)
   tanh NaN     = NaN
   tanh NegInf  = -1
   tanh PosInf  = 1
   tanh (Val x) = Val (tanh x)
   cosh NaN     = NaN
   cosh NegInf  = PosInf
   cosh PosInf  = PosInf
   cosh (Val x) = Val (cosh x)
   asinh NaN     = NaN
   asinh NegInf  = NaN
   asinh PosInf  = PosInf
   asinh (Val x) = Val (asinh x)
   atanh NaN     = NaN
   atanh NegInf  = NaN
   atanh PosInf  = NaN
   atanh (Val x) = Val (atanh x)
   acosh NaN     = NaN
   acosh NegInf  = NaN
   acosh PosInf  = NaN
   acosh (Val x) = Val (acosh x)

instance (Real a, Ord a) => Real (ExtFrac a) where
   toRational NaN     = 0 % 0
   toRational NegInf  = (-1) % 0
   toRational PosInf  = 1 % 0
   toRational (Val x) = toRational x

instance (RealFrac a, Ord a) => RealFrac (ExtFrac a) where
   properFraction NaN     = (1, NaN)
   properFraction NegInf  = (-1, NaN)
   properFraction PosInf  = (1, NaN)
   properFraction (Val x) = let (n,f) = properFraction x in (n, Val f)

instance (RealFloat a, Ord a) => RealFloat (ExtFrac a) where
   floatRadix _ = floatRadix (undefined :: a)
   floatDigits _ = floatDigits (undefined :: a)
   floatRange _ = floatRange (undefined :: a)
   decodeFloat NaN     = error "ExtFrac.decodeFloat"
   decodeFloat NegInf  = error "ExtFrac.decodeFloat"
   decodeFloat PosInf  = error "ExtFrac.decodeFloat"
   decodeFloat (Val x) = decodeFloat x
   encodeFloat i j = Val (encodeFloat i j)
   scaleFloat _ NaN     = NaN
   scaleFloat n NegInf | n < 0     = PosInf
                       | n > 0     = NegInf
                       | otherwise = NaN
   scaleFloat n PosInf | n < 0     = NegInf
                       | n > 0     = PosInf
                       | otherwise = NaN
   scaleFloat n (Val x) = Val (scaleFloat n x)
   isNaN NaN     = True
   isNaN NegInf  = False
   isNaN PosInf  = False
   isNaN (Val x) = isNaN x
   isInfinite NaN     = True
   isInfinite NegInf  = False
   isInfinite PosInf  = False
   isInfinite (Val x) = isInfinite x
   isDenormalized NaN     = False
   isDenormalized NegInf  = False
   isDenormalized PosInf  = False
   isDenormalized (Val x) = isDenormalized x
   isNegativeZero NaN     = False
   isNegativeZero NegInf  = False
   isNegativeZero PosInf  = False
   isNegativeZero (Val x) = isNegativeZero x
   isIEEE _ = isIEEE (0)
   atan2 x y = case x / y of NaN    -> NaN
                             NegInf -> -(pi / 2)
                             PosInf -> pi / 2
                             Val z  -> Val (atan z)

