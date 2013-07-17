{-# LANGUAGE TypeFamilies #-}

module Interpreter.Golog2Test where

import Interpreter.Golog2

instance BAT Int where
   data Sit Int = S0 | Do Int (Sit Int) deriving Show
   s0 = S0
   do_ = Do
   poss a _ = even a

instance DTBAT Int where
   reward a (Do a' _) | a == a'= -1
   reward a _                  = fromIntegral (abs a)
   lookahead _ = 2

p = PseudoAtom . Atom . Prim
q = PseudoAtom . Complex

-- let pp = p 8 `Seq` Nondet [p 2 `Seq` p 4, p 6, p 0 `Seq` p 0 `Seq` p 10] :: Prog Int ; cc = treeDT pp s0 ; sit (Conf _ s) = s in mapM_ (putStrLn.show) $ map sit $ concat $ iterate (concat . map trans) [cc]
-- let pp = p 8 `Seq` Nondet [p 2 `Seq` p 4, p 6, p 0 `Seq` p 0 `Seq` p 10 `Seq` (Interpreter.Golog2Util.star (p 2))] :: Prog Int ; cc = treeDT 4 pp s0 in mapM_ (\cs -> mapM_ (\c -> putStrLn $ show (sit c) ++ " " ++ show (final c)) cs >> putStrLn "") $ take 10 $ do1 cc
