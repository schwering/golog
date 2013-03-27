{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import qualified RSTC.Car as Car
import Interpreter.Golog
import Interpreter.Tree
import Interpreter.TreeUtil
import qualified RSTC.BAT as BAT
import qualified RSTC.Obs as Obs
import RSTC.Progs
import RSTC.Theorems

import Data.Maybe
import Control.Applicative
import Text.Printf

class Show a => ShowPart a where
   showPart :: Int -> a -> String
   partSize :: a -> Int

   showPart _ = show
   partSize _ = 0

instance Show a => Show (BAT.Prim a) where
   show (BAT.Wait t) = "Wait " ++ (show t)
   show (BAT.Accel b q) = "Accel " ++ (show b) ++ " " ++ (show q)
   show (BAT.LaneChange b l) = "LaneChange " ++ (show b) ++ " " ++ (show l)
   show (BAT.Init e) = "Init " ++ (show (Obs.time e))
   show (BAT.Prematch e) = "Prematch " ++ (show (Obs.time e))
   show (BAT.Match e) = "Match " ++ (show (Obs.time e))
   show BAT.Abort = "Abort"
   show BAT.NoOp = "NoOp"
   show (BAT.Start b s) = "Start " ++ (show b) ++ " " ++ s
   show (BAT.End b s) = "End " ++ (show b) ++ " " ++ s

instance Show Finality where
   show Final = "Final"
   show Nonfinal = "Nonfinal"

instance Show a => Show (Atom a) where
   show (Prim a)  = "Prim(" ++ (show a) ++ ")"
   show (PrimF _) = "PrimF(...)"
   show (Test _)  = "Test(...)"

instance Show a => Show (PseudoAtom a) where
   show (Atom a) = "Atom(" ++ (show a) ++ ")"
   show (Complex p) = "Complex(" ++ (show p) ++ ")"

instance Show a => Show (Prog a) where
   show (Seq p1 p2) = "Seq(" ++ (show p1) ++ " " ++ (show p2) ++ ")"
   show (Nondet p1 p2) = "Nondet(" ++ (show p1) ++ " " ++ (show p2) ++ ")"
   show (Conc p1 p2) = "Conc(" ++ (show p1) ++ " " ++ (show p2) ++ ")"
   show (Star p) = "Star(" ++ (show p) ++ ")"
   show (Pick _ _ _) = "Pick(...)"
   show (PseudoAtom p) = "PseudoAtom(" ++ (show p) ++ ")"
   show Nil = "nil"

instance Show a => Show (Sit a) where
   show = show . sit2list

instance ShowPart b => Show (Tree a b) where
   show = showTree 0 0

instance ShowPart Double where
instance ShowPart Prim where
instance ShowPart a => ShowPart (BAT.Prim a) where
instance ShowPart Finality where
instance ShowPart a => ShowPart (Atom a) where
instance ShowPart a => ShowPart (PseudoAtom a) where
instance ShowPart a => ShowPart (Prog a) where

instance Show a => ShowPart (Sit a) where
   showPart n s = show (drop n (sit2list s))

instance ShowPart a => ShowPart (Conf a) where
   showPart n (s,r,d,f) = "("++ showPart n s ++", "++ show r ++", "++ show d ++", "++ show f ++")"
   partSize (s,_,_,_) = length (sit2list s)


showTree :: ShowPart b => Int -> Int -> Tree a b -> String
showTree n _ _ | n > 100 = (replicate (2*n) ' ') ++ "...\n"
showTree n _ Empty = (replicate (2*n) ' ') ++ "Empty\n"
showTree n m (Leaf x) = (replicate (2*n) ' ') ++ "Leaf " ++ showPart m x ++ "\n"
showTree n m (Parent x t) = (replicate (2*n) ' ') ++ "Parent " ++ showPart m x ++ "\n" ++ (showTree (n+1) (partSize x) t)
showTree n m (Branch t1 t2) = (replicate (2*n) ' ') ++ "Branch\n" ++ (showTree (n+1) m t1) ++ (showTree (n+1) m t2)
showTree n _ (Sprout _ _ _) = (replicate (2*n) ' ') ++ "Sprout g, <unknown tree>\n"


sit2list :: Sit a -> [a]
sit2list S0 = []
sit2list (Do a s) = (sit2list s) ++ [a]


{-
filterEmpty :: Tree v a -> Tree v a
filterEmpty Empty          = Empty
filterEmpty t @ (Leaf _)   = t
filterEmpty (Parent x t1) = case filterEmpty t1
   of Empty -> Leaf x
      t2    -> Parent x t2
filterEmpty (Branch t1 t2) = case (filterEmpty t1, filterEmpty t2)
   of (Empty, Empty) -> Empty
      (Empty, t)     -> t
      (t, Empty)     -> t
      (t3, t4)       -> Branch t3 t4
filterEmpty (Sprout _ _)   = error "Main.filterEmpty: Sprout"
-}


data Prim = A | B | C | D | E Double deriving Show

instance BAT Prim where
   poss D _ = False
   poss _ _ = True

   reward A s = case s of Do A _ -> 0
                          _      -> 0
   reward B s = case s of Do B _ -> 1
                          _      -> 0
   reward C s = case s of Do C _ -> 2
                          _      -> 0
   reward D s = case s of Do D _ -> 3
                          _      -> 0
   reward (E x) _ = max 0 (-x*x+100)


-- Ist eh falsch, doch nicht von value!
-- Achso, doch, ist ja value 0. Ich Flachpfeife.
--depth t = snd (value 0 t)


printFluents :: (RealFloat a, PrintfArg a) => Sit (BAT.Prim a) -> IO ()
printFluents s = do _ <- printf "start = %.2f\n" (BAT.start s)
                    mapM_ (\(b,c) -> printf "ntg %s %s = %6.2f\n" (show b) (show c) (BAT.ntg s b c)) [(b,c) | b <- Car.cars, c <- Car.cars, b /= c]
                    mapM_ (\(b,c) -> printf "ttc %s %s = %6.2f\n" (show b) (show c) (BAT.ttc s b c)) [(b,c) | b <- Car.cars, c <- Car.cars, b < c]
                    --putStrLn ("start = " ++ show (BAT.start s))
                    --mapM_ (\(b,c) -> putStrLn ("ntg " ++ show b ++ " " ++ show c ++ " = " ++ show (BAT.ntg s b c))) [(b,c) | b <- Car.cars, c <- Car.cars, b /= c]
                    --mapM_ (\(b,c) -> putStrLn ("ttc " ++ show b ++ " " ++ show c ++ " = " ++ show (BAT.ttc s b c))) [(b,c) | b <- Car.cars, c <- Car.cars, b < c]


--lastObs :: Sit (BAT.Prim a) -> (forall b. Obs.Obs a b => Maybe b)
--lastObs (Do (BAT.Match e) s) = Just e
--lastObs S0                   = Nothing


--nextObs :: Obs.Obs a b => Sit (BAT.Prim a) -> Maybe b
--nextObs s = case lastObs s of Just e -> Obs.next e
--                              _      -> Nothing
--nextObs = fmap Obs.next . lastObs

--nextObsNtg :: Sit (BAT.Prim a) -> Car.Car -> Car.Car -> a
--nextObsNtg (Do (BAT.Match e) s) = Obs.ntg (fromJust (Obs.next e))


nextObsNtg :: Sit (BAT.Prim a) -> Car.Car -> Car.Car -> Maybe (NTG a)
nextObsNtg (Do (BAT.Match e) s) b c = case Obs.next e of Just e' -> Just (Obs.ntg e' b c)
                                                         _       -> Nothing
nextObsNtg S0                   _ _ = Nothing


nextObsTtc :: Sit (BAT.Prim a) -> Car.Car -> Car.Car -> Maybe (NTG a)
nextObsTtc (Do (BAT.Match e) s) b c = case Obs.next e of Just e' -> Just (Obs.ttc e' b c)
                                                         _       -> Nothing
nextObsTtc S0                   _ _ = Nothing


main :: IO ()
main =
   let   a = PseudoAtom (Atom (Prim A))
         b = PseudoAtom (Atom (Prim B))
         c = PseudoAtom (Atom (Prim C))
         --d = PseudoAtom (Atom (Prim D))
         e x = PseudoAtom (Atom (Prim (E x)))
         p1 = ((a `Nondet` b `Nondet` c) `Seq` ((a `Seq` a `Seq` c) `Conc` (b `Seq` b `Seq` c))) `Nondet` Star(a `Seq` c `Seq` c `Seq` c `Seq` c)
         p2 = ((a `Nondet` b) `Seq` ((a `Seq` a `Seq` c) `Conc` (b `Seq` b `Seq` c)))
         --ppick p = Pick (\f -> pso 10 25 5 defaultParams (-20, 20) (Max (fst.f))) 10 p
         ppick p l = Pick (value l) (\f -> picknum (-20, 20) f) p
         p3 i = ppick e i
         p4 i = (ppick e i) `Seq` (ppick e i)
         p5 i = ppick (\x -> (e x) `Seq` (ppick e i)) i
         p6 i = ppick (\x -> (e x) `Seq` (ppick (\y -> e (x*y)) i)) i
         --p = Star(a)
         t1 = tree p1 S0 0.0 0
         t2 = tree p2 S0 0.0 0
   in do
         putStrLn ((show . cutoff 2) t2)
         putStrLn "-------------------------------------------------------\n"
         putStrLn (maybe "nothing" (show . cutoff 3) (trans 0 t2))
         putStrLn "-------------------------------------------------------\n"
         putStrLn (maybe "nothing" (show . cutoff 3) (trans 1 t2))
         putStrLn "-------------------------------------------------------\n"
         putStrLn (maybe "nothing" (show . cutoff 3) (trans 2 t2))
         putStrLn "-------------------------------------------------------\n"
         putStrLn (maybe "nothing" (show . cutoff 3) (trans 3 t2))
         putStrLn "-------------------------------------------------------\n"
         putStrLn ((show . cutoff 10) t1)
         putStrLn "-------------------------------------------------------\n"
         putStrLn (maybe "nothing" (show . cutoff 10) (trans 100000 t1))
         putStrLn "-------------------------------------------------------\n"
         mapM_ (\i -> putStrLn (show i ++ ": " ++ show (do1 i p2 S0))) [0..9]
         putStrLn "-------------------------------------------------------"
         mapM_ (\i -> putStrLn ("Pick one: " ++ show i ++ ": " ++ show (do1 i (p3 i) S0))) [0..5]
         putStrLn "-------------------------------------------------------"
         mapM_ (\i -> putStrLn ("Pick seq: " ++ show i ++ ": " ++ show (do1 i (p4 i) S0))) [0..5]
         putStrLn "-------------------------------------------------------"
         mapM_ (\i -> putStrLn ("Pick nested: " ++ show i ++ ": " ++ show (do1 i (p5 i) S0))) [0..5]
         putStrLn "-------------------------------------------------------"
         mapM_ (\i -> putStrLn ("Pick nested 2: " ++ show i ++ ": " ++ show (do1 i (p6 i) S0))) [0..5]
         putStrLn "-------------------------------------------------------"
{-
-}
         --mapM_ (putStrLn . show) (take 30 Obs.observations)
         --putStrLn "-------------------------------------------------------"
         --mapM_ (\i -> putStrLn ((show . cutoff i) (tree (obsprog Obs.observations) S0 0.0 0))) [0..30]
         --putStrLn "-------------------------------------------------------\n"
-- {-
         let obs      = take 100 Obs.observations
             obsProg  = obsprog obs
             candProg = overtake Car.H Car.D
             prog     = Conc obsProg candProg
             confs    = do3 4 prog S0
         putStrLn (show (force (tree prog S0 0 0)))
         --putStrLn (show (pickbest 4 (tree prog S0 0.0 0)))
         --mapM_ (\(s,v,d,t) -> do putStrLn (show s)
         --                        --putStrLn (show (v, d))
         --                        --printFluents s
         --                        putStrLn (show t)
         --                        putStrLn ""
         --      ) [head confs]
         putStrLn "-------------------------------------------------------"
-- -}
{-
         mapM_ (putStrLn . show) (map (maybe Nothing $ (\x -> Just
               ( x
               , Obs.time x
               , map (Obs.lane x) [Car.B,Car.D,Car.H]
               , map (uncurry (Obs.ntg x)) [(x,y) | x <- [Car.B,Car.D,Car.H], y <- [Car.B,Car.D,Car.H]]
               , map (uncurry (Obs.ttc x)) [(x,y) | x <- [Car.B,Car.D,Car.H], y <- [Car.B,Car.D,Car.H]]
               ))) (take 30 observations))
-}
--         putStrLn "-------"
--         putStrLn (show (cutoff 30 (trans 14 t1)))
--         putStrLn "-------"
--         putStrLn (show (cutoff 30 (trans 24 (trans 14 t1))))
--         putStrLn "-------"
--         putStrLn (show (cutoff 30 (trans 28 (trans 24 (trans 14 t1)))))
--         putStrLn "-------"
--         putStrLn (show (cutoff 30 (trans 42 (trans 24 (trans 24 (trans 14 t1))))))
--         putStrLn "-------"
--         putStrLn (show (cutoff 7 (trans 3 t2)))
--         --putStrLn "-------"
--         --putStrLn (show (toList (cutparents t)))

{-
mydo3 :: (Show a, BAT a) => Depth -> Prog a -> Sit a -> [(Sit a, Reward, Depth)]
mydo3 l p s = mydo4 l (pickbest l (tree p s 0.0 0))


mydo4 :: (Show a) => Depth -> SitTree a -> [(Sit a, Reward, Depth)]
mydo4 l t | final t   = [(sit t, rew t, depth t)]
          | otherwise = case trans l t of
                             Nothing -> []
                             Just t'  -> (sit t', rew t', depth t') : mydo4 l t'
{-
   where bla x @ (Just t') | depth t' >= 35 = Just (Car.debug' ((show (value l t')) ++ " " ++ (show (depth t'))) t')
                           | otherwise = x
         bla x @ Nothing = x
-}
-}

