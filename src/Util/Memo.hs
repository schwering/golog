-- | Simple memoization.
-- To avoid expensive equality arguments the stable names of arguments can be
-- stored in the table, see 'System.Mem.StableName' and [1].
-- It is essentially copied from 'Data.MemoUgly'.
--
-- \[1\]  Simon Peyton Jones, Simon Marlow, Conal Elliott: Stretching the
-- storage manager: weak pointers and stable names in Haskell

module Util.Memo (memoMap, memoMapIO,
                  memoIntMap, memoIntMapIO,
                  memoOblivious, memoObliviousIO,
                  stableNameAndHash, stableNameAndHash1of2, stableNameAndHash1of3,
                  stableName, stableName1of2, stableName1of3,
                  hashStableName, hashStableName1of2, hashStableName1of3,
                  curry3, uncurry3,
                  combine) where

import Prelude hiding (map)
import Control.Concurrent.MVar
import qualified Data.Array.IO as A
import Data.Bits
import Data.Ix
import qualified Data.Map as Map
import qualified Data.IntMap.Strict as IntMap
import System.IO.Unsafe (unsafePerformIO)
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


-- | Memoize the given function by allocating a memo map, and then updating
-- the memo map on each function call.
--
-- The first argument can be used to create a compact representation which
-- provides a sufficient condition for equality (e.g., with stable names).
--
-- The second argument is the memoized function.
memoMapIO :: Ord k => (a -> IO k) -> (a -> b) -> IO (a -> IO b)
memoMapIO k f =
   do var <- newMVar Map.empty
      let f' x = do map <- readMVar var
                    key <- k x
                    case Map.lookup key map of
                         Nothing -> do let y = f x
                                       modifyMVar_ var (return . Map.insert key y)
                                       return y
                         Just y  -> do return y
      return f'


-- | The pure version of 'memoMapIO'.
memoMap :: Ord k => (a -> IO k) -> (a -> b) -> (a -> b)
memoMap k f = let f' = unsafePerformIO (memoMapIO k f)
              in \x -> unsafePerformIO (f' x)


-- | Memoize the given function by allocating a memo hashtable, and then
-- updating the memo table on each function call.
--
-- The first argument can be used to create a compact representation which
-- provides a sufficient condition for equality (e.g., with stable names).
--
-- The second argument is the memoized function.
memoIntMapIO :: Eq k => (a -> IO k) -> (k -> IntMap.Key) -> (a -> b) -> IO (a -> IO b)
memoIntMapIO k h f =
   do var <- newMVar IntMap.empty
      let f' x = do tbl <- readMVar var
                    key <- k x
                    let hash = h key
                    case IntMap.lookup hash tbl of
                         Just (key', y) | key' == key -> return y
                         _                            -> do let y = f x
                                                            modifyMVar_ var (return . IntMap.insert hash (key, y))
                                                            return y
      return f'


-- | The pure version of 'memoIntMapIO'.
memoIntMap :: Eq k => (a -> IO k) -> (k -> IntMap.Key) -> (a -> b) -> (a -> b)
memoIntMap k h f = let f' = unsafePerformIO (memoIntMapIO k h f)
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


stableNameAndHash :: a -> IO (SN a)
stableNameAndHash x = do x' <- makeStableName x
                         let h = hashStableName x'
                         return (SN x' h)


stableNameAndHash1of2 :: (a, b) -> IO (SN a, b)
stableNameAndHash1of2 (x, y) = do x' <- makeStableName x
                                  let h = hashStableName x'
                                  return (SN x' h, y)


stableNameAndHash1of3 :: (a, b, c) -> IO (SN a, b, c)
stableNameAndHash1of3 (x, y, z) = do x' <- makeStableName x
                                     let h = hashStableName x'
                                     return (SN x' h, y, z)


stableName :: a -> IO (StableName a)
stableName = makeStableName


stableName1of2 :: (a, b) -> IO (StableName a, b)
stableName1of2 (x, y) = do x' <- makeStableName x
                           return (x', y)


stableName1of3 :: (a, b, c) -> IO (StableName a, b, c)
stableName1of3 (x, y, z) = do x' <- makeStableName x
                              return (x', y, z)


hashStableName1of2 :: (StableName a, b) -> (Int, b)
hashStableName1of2 (x, y) = (hashStableName x, y)


hashStableName1of3 :: (StableName a, b, c) -> (Int, b, c)
hashStableName1of3 (x, y, z) = (hashStableName x, y, z)


curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z =  f (x, y, z)


uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) =  f x y z


-- | Combines two given hash values.
-- Copied from Data.Hashable.
combine :: Int -> Int -> Int
combine h1 h2 = (h1 + h1 `shiftL` 5) `xor` h2

