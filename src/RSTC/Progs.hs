module RSTC.Progs where

import RSTC.BAT

interpol :: (Fractional a, Ord a) => (a -> a) -> a -> a -> a -> Maybe a
interpol f lo hi goal
   | f lo <= goal && goal <= f hi  = let m = (goal - (f lo)) / ((f hi) - (f lo))
                                     in Just (lo + m * (hi - lo))
   | f lo > f hi                   = interpol (\x -> -(f x)) lo hi (-goal)
   | otherwise                     = Nothing

