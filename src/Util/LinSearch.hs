module Util.LinSearch where

linsearch :: (Fractional a, Fractional b, Real b) =>
             (a -> b) -> (a, a) -> b -> Maybe a
linsearch f (lo, hi) y
   | f lo > f hi  = linsearch (\x -> -(f x)) (lo, hi) (-y)
   | f lo == f hi = if f lo == y then Just lo else Nothing
   | otherwise    = if f lo <= y && y <= f hi
                    then let r  = (y - (f lo)) / ((f hi) - (f lo))
                         in Just (lo + (realToFrac r) * (hi - lo))
                    else Nothing

