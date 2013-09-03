{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Main where

import Prelude hiding (floor, until)
import Data.List (delete)
import Golog.Interpreter
import Golog.Macro
import Golog.Util

type Floor = Int

floors :: [Floor]
floors = [1..10]

newtype TestFormula = TestFormula { phi :: Sit A -> Bool }

instance Show TestFormula where
   show _ = "TestFormula <...>"

instance Read TestFormula where
   readsPrec _ = error "TestFormula is not readable"

data A = Up | Down | TurnOff | ToggleFan | Ring |
         TurnOn Floor | Heat | Cold | Smoke | Reset |
         Wait | Test TestFormula
   deriving (Show, Read)

instance TestAction A where
   testAction = Test . TestFormula
   isTest (Test _) = True
   isTest _        = False

instance BAT A where
   data Sit A = State { floor     :: Floor,
                        temp      :: Int,
                        fan       :: Bool,
                        alarm     :: Bool,
                        onButtons :: [Floor],
                        syncable  :: Bool,
                        rew       :: Reward A }
      deriving Show

   s0 = State { floor = 7, temp = 0, fan = False, alarm = False, onButtons = [3, 5], syncable = False, rew = Reward (0, 0) }

   do_ Up         s = update $ s{floor = floor s + 1}
   do_ Down       s = update $ s{floor = floor s - 1}
   do_ TurnOff    s = update $ s{onButtons = delete (floor s) (onButtons s)}
   do_ ToggleFan  s = update $ s{fan = not (fan s)}
   do_ Ring       s = update $ s
   do_ (TurnOn n) s = update $ s{onButtons = if n `elem` onButtons s then onButtons s else n : onButtons s}
   do_ Heat       s = update $ s{temp = temp s + 1}
   do_ Cold       s = update $ s{temp = temp s - 1}
   do_ Smoke      s = update $ s{alarm = True}
   do_ Reset      s = update $ s{alarm = False}
   do_ Wait       s = (update $ s){syncable = True}
   do_ (Test _)   s = update $ s

   poss Up         s = floor s < 10
   poss Down       s = floor s > 1
   poss TurnOff    s = floor s `elem` onButtons s
   poss ToggleFan  _ = True
   poss Ring       _ = True
   poss (TurnOn _) _ = True
   poss Heat       _ = True
   poss Cold       _ = True
   poss Smoke      _ = True
   poss Reset      _ = True
   poss Wait       _ = True
   poss (Test x)   s = phi x s

instance DTBAT A where
   newtype Reward A = Reward (Int, Int) deriving (Eq, Ord, Show)
   reward = rew

rew' :: Sit A -> Reward A
rew' s = Reward (x, y)
   where m  = length floors
         bs = onButtons s
         x  = m - length bs
         y  = m - minimum (0 : map (\n -> abs (n - floor s)) bs)

update :: Sit A -> Sit A
update s = s{rew = rew' s, syncable = False}

on :: Floor -> Sit A -> Bool
on n s = n `elem` onButtons s

goFloor :: Floor -> Prog A
goFloor n = until (\s -> floor s == n)
               (if_ (\s -> floor s < n)
                  (then_ (prim Up))
                  (else_ (prim Down)))

control :: Prog A
--control = monitor [ prim ToggleFan, prim Ring `Seq` prim Ring ]
--control = monitor [ prim ToggleFan, while alarm (prim Ring) ]
control = monitor [ when (\s -> temp s < -2 && fan s) (prim ToggleFan)
                  , when (\s -> temp s > 2 && not (fan s)) (prim ToggleFan)
                  , while alarm (prim Ring)
                  , until (null.onButtons) (forSome floors (\n ->
                     test (\s -> n `elem` onButtons s) `Seq`
                     goFloor n `Seq`
                     prim TurnOff))
                  , goFloor 1
                  , loop (prim Wait)
                  ]

exog :: (BAT a, Read a) => Sit a -> IO (Sit a)
exog s = do str <- getLine
            if not (null str)
               then return (do_ (read str) s)
               else return s

instance IOBAT A IO where
   syncA a@(Test _) s = return (do_ a s)
   syncA a@Ring     s = putStrLn "Alarm\a" >> exog (do_ a s)
   syncA a          s = putStrLn (show a ++": "++ show (do_ a s)) >> exog (do_ a s)

main :: IO ()
main = do --putStrLn $ show control
          _ <- dooSync' (treeNDIO control s0)
          return ()

