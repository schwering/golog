{-# LANGUAGE ForeignFunctionInterface #-}

-- | Partical swarm optimization implemented in C.
--
-- See the @native_pso.c@ file for details.

module Util.NativePSO (Params(..), Objective(..), Optimum,
                       defaultParams, pso) where

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


pso :: Seed -> IterationCount -> ParticleCount -> Params ->
       Bounds -> Objective -> Optimum
pso seed m n params bounds (Min f) = pso seed m n params bounds (Max (negate.f))
pso seed m n params bounds (Max f) =
      realToFrac (c_pso s' m' n' iw cw sp lo hi fW)
   where s' = fromIntegral seed
         m' = fromIntegral m
         n' = fromIntegral n
         iw = realToFrac (inertiaWeight params)
         cw = realToFrac (cognitiveParam params)
         sp = realToFrac (socialParam params)
         lo = realToFrac (fst bounds)
         hi = realToFrac (snd bounds)
         f' = (\x -> realToFrac (f (realToFrac x)))
         fW = unsafePerformIO (wrap f')


foreign import ccall "wrapper"
   wrap :: (CDouble -> CDouble) -> IO (FunPtr (CDouble -> CDouble))


foreign import ccall "pso"
   c_pso :: CInt -> CInt -> CInt ->          -- seed, m, n
            CDouble -> CDouble -> CDouble -> -- params
            CDouble -> CDouble ->            -- bounds
            FunPtr (CDouble -> CDouble) ->   -- objective (max)
            CDouble

