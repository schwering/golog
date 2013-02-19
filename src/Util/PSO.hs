-- | Partical swarm optimization.
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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Util.PSO where

import System.Random

type ParticleCount = Int
type IterationCount = Int

data Params a = Params { inertiaWeight   :: a,
                         cognitiveWeight :: a,
                         socialParam     :: a }

type Bounds a = (a, a)

data Objective a b = Min (a -> b)
                   | Max (a -> b)

type ObjectiveF a b = a -> b

type Optimum a = a

data Particle a = Particle { position     :: a,
                             velocity     :: a,
                             bestPosition :: a }

type Particles a = [Particle a]


class VectorSpace v a where
   one   :: v a
   zero  :: v a
   inv   :: v a -> v a
   plus  :: v a -> v a -> v a
   minus :: v a -> v a -> v a
   smult :: a -> v a -> v a


data Vector1 a = Vector1 a deriving (Eq, Ord)


instance (Num a) => VectorSpace Vector1 a where
   zero = Vector1 0
   one  = Vector1 1
   inv (Vector1 x) = Vector1 (-x)
   (Vector1 x) `plus`  (Vector1 y) = Vector1 (x + y)
   (Vector1 x) `minus` (Vector1 y) = Vector1 (x - y)
   x           `smult` (Vector1 y) = Vector1 (x * y)


instance (Num a, Enum a) => VectorSpace [] a where
   zero        = [0..] -- TODO XXX bullshit, we somehow
   one         = [1..] -- need to encode the vector size
   inv x       = map (\z -> -z) x
   x `plus`  y = zipWith (+) x y
   x `minus` y = zipWith (-) x y
   x `smult` y = map (x*) y


instance (Random a) => Random (Vector1 a) where
   randomR (Vector1 lo, Vector1 hi) g = let (x, g') = randomR (lo, hi) g
                                        in (Vector1 x, g')
   random g = let (x, g') = random g
              in (Vector1 x, g')


defaultParams :: (RealFloat a) => Params a
defaultParams = Params { inertiaWeight   = 0.729
                       , cognitiveWeight = 1.49445
                       , socialParam     = 1.49445 }


update :: (Num a, VectorSpace v a, Ord (v a), Ord b, Random a, RandomGen gen) =>
   Params a -> Bounds (v a) -> (v a -> b) ->
   (Particle (v a), Optimum (v a), gen) ->
   (Particle (v a), Optimum (v a), gen)
update (Params iw cp sp) (lo, hi) f (Particle x v p, opt, gen0) =
   let (rp, gen1) = randomR (0, 1) gen0
       (rg, gen2) = randomR (0, 1) gen1
       v'         = (iw `smult` v) `plus`
                    ((cp * rp) `smult` (p `minus` x)) `plus`
                    ((sp * rg) `smult` (opt `minus` x))
       x'         = x `plus` v'
       (p', opt') = if lo <= x' && x' <= hi && f x' > f p
                    then (x', if f x' > f opt then x' else opt)
                    else (x, opt)
   in (Particle x' v' p', opt', gen2)


iteration ::
   (Num a, VectorSpace v a, Ord (v a), Ord b, Random a, RandomGen gen) =>
   Params a -> Bounds (v a) -> (v a -> b) ->
   (Particles (v a), Optimum (v a), gen) ->
   (Particles (v a), Optimum (v a), gen)
iteration _      _      _ ([],     opt, gen) = ([], opt, gen)
iteration params bounds f ((p:ps), opt, gen) = ((p':ps'), opt'', gen'')
   where (p', opt', gen')    = update    params bounds f (p, opt, gen)
         (ps', opt'', gen'') = iteration params bounds f (ps, opt', gen')


iterations ::
   (Num a, VectorSpace v a, Ord (v a), Ord b, Random a, RandomGen gen) =>
   IterationCount -> Params a -> Bounds (v a) -> (v a -> b) ->
   (Particles (v a), Optimum (v a), gen) ->
   (Particles (v a), Optimum (v a), gen)
iterations m params bounds f (ps0, opt0, gen0)
      | m == 1    = (ps1, opt1, gen1)
      | m > 1     = (ps2, opt2, gen2)
      | otherwise = error "PSO.iterations: m < 1"
   where (ps1, opt1, gen1) = iteration params bounds f (ps0, opt0, gen0)
         (ps2, opt2, gen2) = iterations (m-1) params bounds f (ps1, max opt0 opt1, gen1)


initial :: (Num a, VectorSpace v a, Ord (v a), Ord b,
            Random a, Random (v a), RandomGen gen) =>
   ParticleCount -> Params a -> Bounds (v a) -> (v a -> b) -> gen ->
   (Particles (v a), gen)
initial n params bounds @ (lo, hi) f gen0
      | n == 1    = ([p], gen2)
      | n > 1     = let (ps, gen3) = initial (n-1) params bounds f gen2
                    in ((p:ps), gen3)
      | otherwise = error "PSO.initial: n < 1"
   where p = Particle x v x
         (x, gen1) = randomR (zero, one) gen0
         (v, gen2) = randomR (lo `minus` hi, hi `minus` lo) gen1


pso :: (Num a, VectorSpace v a, Ord (v a), Num b, Ord b,
        Random a, Random (v a), RandomGen gen) =>
   IterationCount -> ParticleCount -> Params a -> Bounds (v a) ->
   Objective (v a) b -> gen -> (Optimum (v a), gen)
pso m n params bounds (Min f) gen0 = pso m n params bounds (Max f') gen0
   where f' x = -(f x)
pso m n params bounds (Max f) gen0 = (opt2, gen2)
   where (_, opt2, gen2) = iterations m params bounds f (ps, opt1, gen1)
         opt1            = snd (maximum (map (\(Particle x _ _) -> (f x, x)) ps))
         (ps, gen1)      = initial n params bounds f gen0

