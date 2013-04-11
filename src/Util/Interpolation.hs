module Util.Interpolation (Canonical,
                           canonicalize, canonicalizeRecip,
                           nullAt) where

import Debug.Trace

data Canonical a b = Canonical (a -> b)


-- | A simple approximation of infinity.
-- Using IEEE(-like?) infinity as provided by 'Double' is not that good an idea
-- because in some function this leads to @NaN@s.
posInf :: Num a => a
posInf = 100000000000000000


-- | Find the single root of a canicalized function.
--
-- Use 'canonicalize' and 'canonicalizeRecip' in order to search for specific
-- Y-values other than 0.
nullAt :: (Fractional a, Fractional b) => (b -> a) -> Canonical a b -> a
nullAt domToDef (Canonical f) = lo + (domToDef r) * (hi - lo)
   where r       = (y - (f lo)) / ((f hi) - (f lo))
         y       = 0
         (lo,hi) = (-1, 1)


-- Canonicalizes the reciprocal function @f(x) = x*C1 + C2@.
--
-- Computing the canonicalized function needs no evaluation of @f@.
-- Per each evaluation of the canonicalized function, @f@ is evaluated once.
canonicalize :: Num b => (a -> b) -> b -> Canonical a b
canonicalize f y = Canonical (\x -> f x - y)


-- Canonicalizes the reciprocal function @f(x) = 1 / (x*C1 + C2) + C3@.
--
-- @
--       f(x) = y
-- <=>   1 / (x*C1 + C2) + C3 = y
-- <=>   1 / (x*C1 + C2) = y - C3
-- <=>   1 = (y - C3) * (x*C1 + C2)      = (y - C3) * 1 / (f(x) - C3)
-- <=>   0 = (y - C3) * (x*C1 + C2) - 1  = (y - C3) * 1 / (f(x) - C3) - 1
--
-- C3 = f(+inf)
-- C2 = 1 / (f(0) - C3
-- @
--
-- Computing the canonicalized function costs one evaluation of @f@.
-- Per each evaluation of the canonicalized function, @f@ is evaluated once
-- more.
canonicalizeRecip :: (Num a, Fractional b) => (a -> b) -> b -> Canonical a b
canonicalizeRecip f y = Canonical (\x -> (y - c3) * 1 / (f x - c3) - 1)
   where c3 = f posInf

