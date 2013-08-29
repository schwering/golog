module Util.Interpolation (Canonical, Growth(..),
                           canonicalize, canonicalizeSum,
                           nullAt) where

data Growth = Linear | Recip deriving Show
data Canonical a b = Lin b b | Quad b b b deriving Show


-- | A simple approximation of infinity.
-- Using IEEE(-like?) infinity as provided by 'Double' is not that good an idea
-- because in some function this leads to @NaN@s.
posInf :: Num a => a
posInf = 1000000000000


-- | Find the single root of a canicalized function.
--
-- Use 'canonicalize' and 'canonicalizeRecip' in order to search for specific
-- Y-values other than 0.
nullAt :: (Fractional a, RealFloat b) => (b -> a) -> Canonical a b -> [a]
nullAt domToDef (Lin a b) = map domToDef (filter (not.isNaN) [x])
   where x = -b / a
nullAt domToDef (Quad a b c) = map domToDef (filter (not.isNaN) [x1, x2])
   where x1 = (-b + sqrt (b*b - 4*a*c)) / (2*a)
         x2 = (-b - sqrt (b*b - 4*a*c)) / (2*a)


-- Canonicalizes functions.
--
-- If the type is @Linear@, the function must be of type @f(x) = x*C1 + C2@.
-- Canonicalizing a linear @f@ costs two evaluations of @f@.
--
-- If the type is @Recip@ the function must be @f(x) = 1 / (x*C1 + C2) + C3@.
-- @
--       f(x) = y
-- <=>   1 / (x*C1 + C2) + C3 = y
-- <=>   1 / (x*C1 + C2) = y - C3
-- <=>   1 = (y - C3) * (x*C1 + C2)      = (y - C3) * 1 / (f(x) - C3)
-- <=>   0 = (y - C3) * (x*C1 + C2) - 1  = (y - C3) * 1 / (f(x) - C3) - 1
--
-- C3 = f(+inf)
-- C2 = 1 / (f(0) - C3)
-- C1 = 1 / (f(1) - C3) - C2
-- @
-- Canonicalizing a linear @f@ costs three evaluations of @f@.
canonicalize :: (Num a, Fractional b) => Growth -> (a -> b) -> b -> Canonical a b
canonicalize Linear f y = Lin a b
   where a = f 1 - b - y
         b = f 0 - y
canonicalize Recip f y = canonicalize Linear f' 0
   where c3   = f posInf
         f' x = (y - c3) * 1 / (f x - c3) - 1


-- Canonicalizes the sum of two functions.
--
-- If the type is @Linear@, the function must be of type @f(x) = x*C1 + C2@.
-- Canonicalizing the sum of two linear @f@, @g@ costs two evaluations of @f@
-- and two evaluations of @g@.
--
-- If the type is @Recip@ the function must be @f(x) = 1 / (x*C1 + C2) + C3@.
-- @
--       f(x) + g(x) = y
-- <=>   1 / (x*C1 + C2) + C3 + 1 / (x*D1 + D2) + D3 = y
-- <=>   1 / (x*C1 + C2) + 1 / (x*D1 + D2) = y - C3 - D3
-- <=>   (x*C1 + C2) + (x*D1 + D2) = (y - C3 - D3) * (x*C1 + C2) * (x*D1 + D2)
-- <=>   0 = (y - C3 - D3) * (x*C1 + C2) * (x*D1 + D2) - (x*C1 + C2) - (x*D1 + D2)
-- <=>   0 = (y - C3 - D3) * (x^2*C1*D1 + x*(C1*D2+C2*D1) + C2*D2) - (x*C1 + C2) - (x*D1 + D2)
-- <=>   0 = x^2*C1*D1*(y-C3-D3) + x*(C1*D2*(y-C3-D3) + C2*D1*(y-C3-D3) - C1 - D1) + C2*D2*(y-C3-D3) - C2 - D2
--
-- C3 = f(+inf)
-- C2 = 1 / (f(0) - C3)
-- C1 = 1 / (f(1) - C3) - C2
-- @
-- Canonicalizing the sum of two reciprocal @f@, @g@ costs three evaluations of
-- @f@ and three evaluations of @g@.
canonicalizeSum :: (Num a, Fractional b) => Growth -> (a -> b) -> (a -> b) -> b -> Canonical a b
canonicalizeSum Linear f g y = canonicalize Linear (\x -> f x + g x) y
canonicalizeSum Recip f g y = Quad a b c
   where a = c1*d1*(y-c3-d3)
         b = c1*d2*(y-c3-d3) + c2*d1*(y-c3-d3) - c1 - d1
         c = c2*d2*(y-c3-d3) - c2 - d2
         c3 = f posInf
         c2 = 1 / (f 0 - c3)
         c1 = 1 / (f 1 - c3) - c2
         d3 = g posInf
         d2 = 1 / (g 0 - d3)
         d1 = 1 / (g 1 - d3) - d2

