{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A simple BAT for manual comparison between the old and new Golog
-- interpreters.
module GologTest where

import Golog.Interpreter
import Golog.Macro
import Golog.Util hiding (HistBAT(..))

instance BAT Int where
   data Sit Int = S0 | Do Int (Sit Int) deriving Show
   s0 = S0
   do_ = Do
   poss a _ = even a

instance DTBAT Int where
   newtype Reward Int = Reward { rew :: (Double, Int) } deriving (Eq, Ord, Show)
   reward (Do _ s)         | sitlen s > 5 = Reward $ rew (reward s) `plus2` (-1000, 1)
   reward (Do a (Do a' s)) | a == a'      = Reward $ rew (reward s) `plus2` (-1, 1)
   reward (Do a s)                        = Reward $ rew (reward s) `plus2` (fromIntegral (max 0 a), 1)
   reward S0                              = Reward $ (0, 0)

plus2 :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
plus2 (u,v) (x,y) = (u+x,v+y)

instance Monad m => IOBAT Int m where
   syncA 4 s = return $ do_ 2 s
   syncA a s = return $ do_ a s

sitlen :: Sit Int -> Int
sitlen S0       = 0
sitlen (Do _ s) = 1 + sitlen s

p = prim
q = atomic

doDT :: DTBAT a => Int -> Prog a -> Sit a -> [Sit a]
doDT l p s = map sit $ dooBFS (treeDT l p s)

testDTIO :: IO (Sit Int)
testDTIO = do  c <- return $ t
               c <- return $ n c -- 2
               -- c <- sync c
               c <- return $ n c -- 4
               -- c <- sync c
               c <- return $ n c -- 4 vs 0
               -- c <- sync c
               c <- return $ n c -- 14 vs 18
               c <- sync c
               putStrLn $ show $ reward (sit c)
               return $ sit c
   where t = treeDTIO 3 (p 2 `Seq` p 4 `Seq` choice [p 4 `Seq` p 16, p 0 `Seq` p 16] :: Prog Int) s0
         n = head.trans
