-- | Partical swarm optimization implemented in C.
--
-- We adhere to the naming conventions used by the English Wikipedia article
-- (among others):
--
--  * X stands for the current position of a certain particle,
--
--  * P stands for the optimal position of a certain particle found thus far,
--
--  * V stands for the current velocity of a certain particle,
--
--  * G stands for the optimal position of all particles found thus far.
--
-- The results seem to be worse than in the Mercury implementation. Maybe there
-- is some bug, possibly unintended re-usage of random number generators? (Btw.,
-- Mercury's destructive modes are really good to avoid this kind of stuff.)
--
-- Also, a more functional implementation might be faster.
-- Or just write it in C? But then how can we call the lambda objective
-- function?

{-# LANGUAGE ForeignFunctionInterface #-}

module Util.NativePSO where

import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe

type Seed = Int
type ParticleCount = Int
type IterationCount = Int

data Params = Params { inertiaWeight  :: Double,
                       cognitiveParam :: Double,
                       socialParam    :: Double }

type Bounds = (Double, Double)

data Objective = Min (Double -> Double)
               | Max (Double -> Double)

type Optimum = Double

defaultParams :: Params
defaultParams = Params { inertiaWeight  = 0.729
                       , cognitiveParam = 1.49445
                       , socialParam    = 1.49445 }


pso :: Seed -> IterationCount -> ParticleCount -> Params -> Bounds -> Objective -> Optimum
pso seed m n params bounds (Min f) = pso seed m n params bounds (Max (negate .f ))
pso seed m n params bounds (Max f) = unsafePerformIO (
      do fW <- wrap f'
         r <- c_pso s' m' n' iw cw sp lo hi fW
         return (realToFrac r)
   )
   where s' = fromIntegral seed
         m' = fromIntegral m
         n' = fromIntegral n
         iw = realToFrac (inertiaWeight params)
         cw = realToFrac (cognitiveParam params)
         sp = realToFrac (socialParam params)
         lo = realToFrac (fst bounds)
         hi = realToFrac (snd bounds)
         f' = (\x -> realToFrac (f (realToFrac x)))


foreign import ccall "wrapper"
  wrap :: (CDouble -> CDouble) -> IO (FunPtr (CDouble -> CDouble))


foreign import ccall
   c_pso :: CInt -> CInt -> CInt ->          -- seed, m, n
            CDouble -> CDouble -> CDouble -> -- params
            CDouble -> CDouble ->            -- bounds
            FunPtr (CDouble -> CDouble) ->   -- objective (max)
            IO CDouble

