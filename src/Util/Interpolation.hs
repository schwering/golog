module Util.Interpolation where

import Debug.Trace

-- | Interpolates a linear function.
-- 'interpolate (lo,hi) y f' returns a 'x' such 'f x = y' holds.
-- This computation only works if 'f' is linear.
interpolateLin :: (Fractional a, Fractional b, Show a, Show b) =>
                  (b -> a) -> (a, a) -> b -> (a -> b) -> a
interpolateLin domToDef (lo, hi) y f = lo + (domToDef r) * (hi - lo)
   where r = (y - (f lo)) / ((f hi) - (f lo))


-- | Interpolate a reciprocal function.
--       f'(x) = y'
-- <=>   1 / (f(x) - C2) = 1 / (y - C2)
-- <=>   f(x) - C2 = y - C2
-- <=>   f(x) = y
interpolateRecipLin :: (Fractional a, Fractional b, Show a, Show b) =>
                       (b -> a) -> (a, a) -> b -> (a -> b) -> a
interpolateRecipLin domToDef bounds y f = interpolateLin domToDef bounds y' f'
   where f' x = 1 / (f x - c)
         y'   = 1 / (y - c)
         c    = f 10000000
         --c    = f (1/0)


interpolateRecipLinAndLinForZero :: (Fractional a, Fractional b, Show a, Show b) =>
                                    (b -> a) -> (a, a) -> (a -> (b, b)) -> a
interpolateRecipLinAndLinForZero domToDef bounds fg = interpolateLin domToDef bounds y' h'
   where f    = fst . fg
         g    = snd . fg
         f' x = 1 / (f x - c)
         g' x = g x - 1 / c
         h' x = f' x + g' x
         y'   = 2 / (-c)
         c    = f 1000000000
         --c    = f (1/0)


   -- Instead of some generic y, we need to search for 0, because otherwise we
   -- can't transform g(x) = y <=> g(x) = y'.
   --
   -- f(x)  = 1/x * C1 + C2                  0
   -- f'(x) = 1 / (f(x) - C2) = x * 1/C1     1/(-C2)
   --
   -- g(x)  = x * C3 + C4                    0
   -- g'(x) = x + C3 + C4 + 1 / (-C2)        1/(-C2)
   --
   --       f'(x) = 1 / (-C2)
   -- <=>   1 / (f(x) - C2) = 1 / (-C2)
   -- <=>   f(x) - C2 = -C2
   -- <=>   f(x) = 0
   --
   --       g'(x) = 1 / (-C2)
   -- <=>   x * C3 + C4 + 1/(-C2) = 1/(-C2)
   -- <=>   x * C3 + C4 = 0
   -- <=>   g(x) = 0

