{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | A simple BAT for manual comparison between the old and new Golog
-- interpreters.
module GologTest where

import Golog.Interpreter
import qualified Golog.Macro as M
import Golog.Util hiding (HistBAT(..))

instance BAT Int where
   data Sit Int = S0 | Do Int (Sit Int) deriving Show
   s0 = S0
   do_ = Do
   poss a _ = even a

instance DTBAT Int where
   reward _ s         | sitlen s > 5 = -1000
   reward a (Do a' _) | a == a'      = -1
   reward a _                        = fromIntegral (max 0 a)

instance Monad m => IOBAT Int m where
   syncA 4 s = return $ do_ 2 s
   syncA a s = return $ do_ a s

sitlen :: Sit Int -> Int
sitlen S0       = 0
sitlen (Do _ s) = 1 + sitlen s

p = M.prim
q = M.atomic
star = M.star
nondet = Nondet
conc = foldl1 Conc
atomic = M.atomic

doDT :: DTBAT a => Depth -> Prog a -> Sit a -> [Sit a]
doDT l p s = map sit $ doo (treeDT l p s)

rewardSum :: Sit Int -> Reward
rewardSum S0       = 0
rewardSum (Do a s) = rewardSum s + reward a s

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
               putStrLn $ show $ rewardSum (sit c)
               return $ sit c
   where t = treeDTIO 3 (p 2 `Seq` p 4 `Seq` nondet [p 4 `Seq` p 16, p 0 `Seq` p 16] :: Prog Int) s0
         n = head.trans
