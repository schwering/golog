-- | Simple memoization.
-- To avoid expensive equality arguments the stable names of arguments can be
-- stored in the table, see 'System.Mem.StableName' and [1].
-- It is essentially copied from 'Data.MemoUgly'.
--
-- \[1\]  Simon Peyton Jones, Simon Marlow, Conal Elliott: Stretching the
-- storage manager: weak pointers and stable names in Haskell

module Util.Memo (memo, memoIO,
                  memoOblivious, memoObliviousIO,
                  stableNameFirstOfThree,
                  hashStableNameFirstOfThree,
                  stableNameAndHashFirstOfThree,
                  curry3, uncurry3) where

import Control.Concurrent.MVar
import qualified Data.Array.IO as A
import Data.Ix
import qualified Data.Map as M
import System.IO.Unsafe(unsafePerformIO)
import System.Mem.StableName


class Ix a => ModIx a where
   mapTo :: a -> (a, a) -> a


intMapTo :: Int -> (Int, Int) -> Int
intMapTo x (lo,hi) = (x `mod` (hi-lo+1)) + lo


instance ModIx Int where
   mapTo = intMapTo


instance (Enum a, Enum b, Enum c, Ix a, Ix b, Ix c) => ModIx (a, b, c) where
   mapTo (x,y,z) ((lo1,lo2,lo3),(hi1,hi2,hi3)) = (f x (lo1,hi1),
                                                  f y (lo2,hi2),
                                                  f z (lo3,hi3))
      where f u (lo,hi) = toEnum (intMapTo (fromEnum u) (fromEnum lo, fromEnum hi))


-- | Stable name together with its hash value.
-- The hash value is kept for efficient ordering.
data SN a = SN (StableName a) Int deriving Eq


instance Ord (SN a) where
   compare (SN _ x) (SN _ y) = compare x y


-- | Memoize the given function by allocating a memo table, and then updating
-- the memo table on each function call.
--
-- The first argument can be used to create a compact representation which
-- provides a sufficient condition for equality (e.g., with stable names).
--
-- The second argument is the memoized function.
memoIO :: (Ord k) => (a -> IO k) -> (a -> b) -> IO (a -> IO b)
memoIO k f =
   do v <- newMVar M.empty
      let f' x = do m <- readMVar v
                    y <- k x
                    case M.lookup y m of
                         Nothing -> do let z = f x
                                       modifyMVar_ v (return . M.insert y z)
                                       return z
                         Just z  -> do return z
      return f'


-- | The pure version of 'memoIO'.
memo :: (Ord k) => (a -> IO k) -> (a -> b) -> (a -> b)
memo k f = let f' = unsafePerformIO (memoIO k f)
           in \x -> unsafePerformIO (f' x)


-- | Memoize the given function by allocating a fixed-size memo table, and then
-- updating the memo table on each function call.
--
-- The first argument can be used to create a compact representation which
-- provides a sufficient condition for equality (e.g., with stable names).
--
-- The second argument maps these elements to an index type.
--
-- The third and fourth arguments denote the range of array.
memoObliviousIO :: (Eq k, ModIx h) => (a -> IO k) -> (k -> h) -> h -> h -> (a -> b) -> IO (a -> IO b)
memoObliviousIO k h lo hi f =
   do let newArray = A.newArray :: Ix i => (i, i) -> e -> IO (A.IOArray i e)
      a <- newArray (lo, hi) Nothing
      let f' x = do key <- k x
                    let hash = mapTo (h key) (lo,hi)
                    e <- A.readArray a hash
                    case e of
                         Just (key', y) | key == key' -> return y
                         _ -> do let y = f x
                                 A.writeArray a hash (Just (key, y))
                                 return y
      return f'


-- | The pure version of 'memoObliviousIO'.
memoOblivious :: (Eq k, ModIx h) => (a -> IO k) -> (k -> h) -> h -> h -> (a -> b) -> (a -> b)
memoOblivious k h lo hi f = let f' = unsafePerformIO (memoObliviousIO k h lo hi f)
                            in \x -> unsafePerformIO (f' x)


stableNameAndHashFirstOfThree :: (a, b, c) -> IO (SN a, b, c)
stableNameAndHashFirstOfThree (x, y, z) = do x' <- makeStableName x
                                             let h = hashStableName x'
                                             return (SN x' h, y, z)


stableNameFirstOfThree :: (a, b, c) -> IO (StableName a, b, c)
stableNameFirstOfThree (x, y, z) = do x' <- makeStableName x
                                      return (x', y, z)


hashStableNameFirstOfThree :: (StableName a, b, c) -> (Int, b, c)
hashStableNameFirstOfThree (x, y, z) = (hashStableName x, y, z)


curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z =  f (x, y, z)


uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) =  f x y z

