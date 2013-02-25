-- | Simple memoization.
-- To avoid expensive equality arguments the stable names of arguments can be
-- stored in the table, see 'System.Mem.StableName' and [1].
-- It is essentially copied from 'Data.MemoUgly'.
--
-- \[1\]  Simon Peyton Jones, Simon Marlow, Conal Elliott: Stretching the
-- storage manager: weak pointers and stable names in Haskell
module Util.Memo (memo, memoIO, stableNameFirstOfThree, curry3, uncurry3) where

import Control.Concurrent.MVar
import qualified Data.Map as M
import System.IO.Unsafe(unsafePerformIO)
import System.Mem.StableName


-- | Stable name together with its hash value.
-- The hash value is kept for efficient ordering.
data SN a = SN (StableName a) Int deriving Eq


instance Ord (SN a) where
   compare (SN _ x) (SN _ y) = compare x y


-- | Memoize the given function by allocating a memo table, and then updating
-- the memo table on each function call.
--
-- The first argument creates the keys that are stored in the table. Mapping
-- these to stable names might make sense.
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
                         Just z  -> return z
      return f'


-- | The pure version of 'memoIO'.
--
-- The first argument creates the keys that are stored in the table. Mapping
-- these to stable names might make sense.
--
-- The second argument is the memoized function.
memo :: (Ord k) => (a -> IO k) -> (a -> b) -> (a -> b)
memo k f = let f' = unsafePerformIO (memoIO k f)
           in \x -> unsafePerformIO (f' x)


stableNameFirstOfThree :: (a, b, c) -> IO (SN a, b, c)
stableNameFirstOfThree (x, y, z) = do x' <- makeStableName x
                                      let h = hashStableName x'
                                      return (SN x' h, y, z)


curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z =  f (x, y, z)


uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) =  f x y z

