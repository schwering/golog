{-# LANGUAGE ForeignFunctionInterface #-}

module Util.MemoCache where

import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import System.IO.Unsafe (unsafePerformIO)


type CacheId = Int


memo1 :: (Real b, Fractional c) => CacheId -> (a -> b) -> a -> c
memo1 id f a = realToFrac r
   where f' x = let x' = unsafePerformIO (deRefStablePtr x)
                in (realToFrac (f x')) :: CDouble
         a' = unsafePerformIO (newStablePtr a)
         r  = unsafePerformIO (c_memo1 id (unsafePerformIO (wrap1 f')) a')


memo2 :: (Enum b, Real c, Fractional d) => CacheId -> (a -> b -> c) -> a -> b -> d
memo2 id f a b = realToFrac r
   where f' x y = let x' = unsafePerformIO (deRefStablePtr x)
                      y' = c2e (y :: CInt)
                  in (realToFrac (f x' y')) :: CDouble
         a' = unsafePerformIO (newStablePtr a)
         b' = e2c b
         r  = unsafePerformIO (c_memo2 id (unsafePerformIO (wrap2 f')) a' b')


memo3 :: (Enum b, Enum c, Real d, Fractional e) => CacheId -> (a -> b -> c -> d) -> a -> b -> c -> e
memo3 id f a b c = realToFrac r
   where f' x y z = let x' = unsafePerformIO (deRefStablePtr x)
                        y' = c2e (y :: CInt)
                        z' = c2e (z :: CInt)
                    in (realToFrac (f x' y' z')) :: CDouble
         a' = unsafePerformIO (newStablePtr a)
         b' = e2c b
         c' = e2c c
         r  = unsafePerformIO (c_memo3 id (unsafePerformIO (wrap3 f')) a' b' c')


e2c :: Enum a => a -> CInt
e2c = fromIntegral . fromEnum


c2e :: Enum a => CInt -> a
c2e = toEnum . fromIntegral


timeCost :: IO Double
timeCost = c_time_cost >>= \cost -> return (realToFrac cost)


ticksCost :: IO Integer
ticksCost = c_ticks_cost >>= \cost -> return (fromIntegral cost)


foreign import ccall "wrapper"
   wrap1 :: (StablePtr a -> CDouble) ->
            IO (FunPtr (StablePtr a -> CDouble))

foreign import ccall "wrapper"
   wrap2 :: (StablePtr a -> CInt -> CDouble) ->
            IO (FunPtr (StablePtr a -> CInt -> CDouble))

foreign import ccall "wrapper"
   wrap3 :: (StablePtr a -> CInt -> CInt -> CDouble) ->
            IO (FunPtr (StablePtr a -> CInt -> CInt -> CDouble))


foreign import ccall "lru_cache1"
   c_memo1 :: CacheId -> FunPtr (StablePtr a -> CDouble) ->
              StablePtr a -> IO CDouble

foreign import ccall "lru_cache2"
   c_memo2 :: CacheId -> FunPtr (StablePtr a -> CInt -> CDouble) ->
              StablePtr a -> CInt -> IO CDouble

foreign import ccall "lru_cache3"
   c_memo3 :: CacheId -> FunPtr (StablePtr a -> CInt -> CInt -> CDouble) ->
              StablePtr a -> CInt -> CInt -> IO CDouble

foreign import ccall "memo_cache_time_cost"
   c_time_cost :: IO CDouble

foreign import ccall "memo_cache_ticks_cost"
   c_ticks_cost :: IO CULong

