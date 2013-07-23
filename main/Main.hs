{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import RSTC.Car
import Interpreter.Golog2
import Interpreter.Golog2Util
--import Interpreter.Tree
--import Interpreter.TreeUtil
import RSTC.BAT.Base
import RSTC.BAT.Progression
import qualified RSTC.Obs as O
import RSTC.Progs
--import RSTC.Theorems
--import Util.Interpolation
import qualified Util.MemoCache

--import Data.List (sortBy)
--import Data.Maybe
--import Control.Applicative
import Text.Printf
import GHC.Stats
--import Data.ByteString.Char8 (pack)
-- import System.Remote.Monitoring

class Show a => ShowPart a where
   showPart :: Int -> a -> String
   partSize :: a -> Int

   showPart _ = show
   partSize _ = 0

instance Show a => Show (Prim (Qty a)) where
   show (Wait t) = "Wait " ++ show (unwrap t)
   show (Accel b q) = "Accel " ++ show b ++ " " ++ show (unwrap q)
   show (LaneChange b l) = "LaneChange " ++ (show b) ++ " " ++ (show l)
   show (Init e) = "Init " ++ show (O.time e :: Double)
   show (Prematch e) = "Prematch " ++ show (O.time e :: Double)
   show (Match e) = "Match " ++ show (O.time e :: Double)
   show Abort = "Abort"
   show NoOp = "NoOp"
   show (Start b s) = "Start " ++ show b ++ " " ++ s
   show (End b s) = "End " ++ show b ++ " " ++ s
   show (Msg s) = "Msg " ++ s

{-
instance Show Finality where
   show Final = "Final"
   show Nonfinal = "Nonfinal"
-}

{-
instance Show a => Show (Atom a) where
   show (Prim a)  = "Prim(" ++ (show a) ++ ")"
   show (PrimF _) = "PrimF(...)"
   show (Test _)  = "Test(...)"
-}

instance Show a => Show (PseudoAtom a) where
   show (Atom a) = "Atom(" ++ (show a) ++ ")"
   show (Complex p) = "Complex(" ++ (show p) ++ ")"

instance Show a => Show (Prog a) where
   show (Seq p1 p2) = "Seq(" ++ (show p1) ++ " " ++ (show p2) ++ ")"
   --show (Nondet p1 p2) = "Nondet(" ++ (show p1) ++ " " ++ (show p2) ++ ")"
   show (Nondet ps) = "Nondet[ " ++ concat (map (\p -> show p ++" ") ps) ++ "]"
   show (Conc p1 p2) = "Conc(" ++ (show p1) ++ " " ++ (show p2) ++ ")"
   --show (Star p) = "Star(" ++ (show p) ++ ")"
   --show (Pick _ _ _) = "Pick(...)"
   show (PseudoAtom p) = "PseudoAtom(" ++ (show p) ++ ")"
   show Nil = "nil"

instance Show (Sit (Prim (Qty Double))) where
   show = show . reverse . sit2list

{-
instance ShowPart c => Show (Tree a b c) where
   show = showTree 0 0
-}

instance ShowPart Double where
--instance ShowPart Prim where
instance ShowPart a => ShowPart (Prim (Qty a)) where
--instance ShowPart Finality where
instance ShowPart a => ShowPart (Atom a) where
instance ShowPart a => ShowPart (PseudoAtom a) where
instance ShowPart a => ShowPart (Prog a) where

instance ShowPart (Sit (Prim (Qty Double))) where
   showPart n s = show (drop n (sit2list s))

{-
instance ShowPart (Conf (Prim (Qty Double))) where
   showPart n (s,r,d,f) = "("++ showPart n s ++", "++ show r ++", "++ show d ++", "++ show f ++")"
   partSize (s,_,_,_) = length (sit2list s)
-}

{-
showTree :: ShowPart c => Int -> Int -> Tree a b c -> String
showTree n _ _ | n > 100 = (replicate (2*n) ' ') ++ "...\n"
showTree n _ Empty = (replicate (2*n) ' ') ++ "Empty\n"
showTree n m (Leaf x) = (replicate (2*n) ' ') ++ "Leaf " ++ showPart m x ++ "\n"
showTree n m (Parent x t) = (replicate (2*n) ' ') ++ "Parent " ++ showPart m x ++ "\n" ++ (showTree (n+1) (partSize x) t)
showTree n m (Branch t1 t2) = (replicate (2*n) ' ') ++ "Branch\n" ++ (showTree (n+1) m t1) ++ (showTree (n+1) m t2)
showTree n _ (Sprout _ _ _) = (replicate (2*n) ' ') ++ "Sprout g, <unknown tree>\n"
-}

{-
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
-}


printFluents :: Sit (Prim (Qty Double)) -> IO ()
printFluents s = case reverse (sit2list s)
   of Match e : _ ->
         do _ <- printf "time = %.2f\n" (unwrap (time s))
            mapM_ (\(b,c) -> do printf "ntg %s %s:   model: %8.2f   obs: %8.2f   \916: %8.2f\n"
                                    (show b) (show c)
                                    (unwrap (ntg s b c))
                                    (O.ntg e b c :: Double)
                                    (unwrap (ntgDiff s e b c))
                  ) [(b,c) | b <- cars, c <- cars, b /= c]
            mapM_ (\(b,c) -> do printf "ttc %s %s:   model: %8.2f   obs: %8.2f   \916: %8.2f\n"
                                    (show b) (show c)
                                    (unwrap (ttc s b c))
                                    (O.ttc e b c :: Double)
                                    (unwrap (ttcDiff s e b c))
                  ) [(b,c) | b <- cars, c <- cars, b < c]
            mapM_ (\b -> do printf "lane %s:    model:    %.5s   obs:    %.5s   \916: %8d\n"
                                    (show b)
                                    (show (lane s b))
                                    (show (O.lane e b))
                                    (if lane s b == O.lane e b then 0 :: Int else 1)
                  ) cars
      _ ->
         do _ <- printf "time = %.2f\n" (unwrap (time s))
            mapM_ (\(b,c) -> do printf "ntg %s %s:   model: %8.2f\n"
                                    (show b) (show c)
                                    (unwrap (ntg s b c))
                  ) [(b,c) | b <- cars, c <- cars, b /= c]
            mapM_ (\(b,c) -> do printf "ttc %s %s:   model: %8.2f\n"
                                    (show b) (show c)
                                    (unwrap (ttc s b c))
                  ) [(b,c) | b <- cars, c <- cars, b < c]
            mapM_ (\b -> do printf "lane %s:    model:    %.5s\n"
                                    (show b)
                                    (show (lane s b))
                  ) cars


traceCsv :: Sit (Prim (Qty Double)) -> [String]
traceCsv ss = header : (reverse (map (concat . interleave delim . map (show.unwrap) . csv) (subsits ss)))
   where subsits s = case history s of Match _ : _ -> s : subsits (predsit s)
                                       _ : _       -> subsits (predsit s)
                                       _           -> []
         header = "time" ++ delim ++
                  concat (interleave delim (
                     ["ntg_S("++show b++","++show c++")" | b <- cars, c <- cars, b /= c] ++
                     ["ttc_S("++show b++","++show c++")" | b <- cars, c <- cars, b < c] ++
                     ["ntg_O("++show b++","++show c++")" | b <- cars, c <- cars, b /= c] ++
                     ["ttc_O("++show b++","++show c++")" | b <- cars, c <- cars, b < c] ++
                     ["ntg_D("++show b++","++show c++")" | b <- cars, c <- cars, b /= c] ++
                     ["ttc_D("++show b++","++show c++")" | b <- cars, c <- cars, b < c]
                  ))
         csv s =
            case history s
            of Match e : _ ->
                  [time s] ++
                  [ntg s b c | b <- cars, c <- cars, b /= c] ++
                  [ttc s b c | b <- cars, c <- cars, b < c] ++
                  [O.ntg e b c | b <- cars, c <- cars, b /= c] ++
                  [O.ttc e b c | b <- cars, c <- cars, b < c] ++
                  [ntgDiff s e b c | b <- cars, c <- cars, b /= c] ++
                  [ttcDiff s e b c | b <- cars, c <- cars, b < c]
               _ -> error "Main.traceCsv: no Match action"


gnuplot :: String -> [String]
gnuplot csvFile =
      ["gnuplot --persist << EOF"
      ,"set termoption dash"
      ,"set yrange [-10:10]"
      ,"set xtics 1"
      ,"set grid"
      ,"set datafile separator \""++delim++"\""
      ,"set key autotitle columnhead"
      --,"set terminal png size 1280,1024"
      ,"set output \"trace-$(date +%Y-%m-%d_%H-%M-%S).svg\""
      ,"set terminal svg dashed"
      ,"plot " ++
         concat (interleave ", " (
            map (\i -> plotCmd i 1 (i-offset) 4) [offset+1..offset+sitData] ++
            map (\i -> plotCmd i 2 (i-sitData-offset) 8) [offset+sitData+1..offset+sitData+obsData] ++
            []-- map (\i -> plotCmd i (i-obsData-sitData-offset) 8) [offset+sitData+obsData+1..offset+sitData+obsData+diffData]
         ))
      --,"set terminal qt"
      ,"set terminal wxt"
      ,"replot"
      ,"EOF"
      ]
   where plotCmd :: Int -> Int -> Int -> Int -> String
         plotCmd i lt lc lw = "\""++csvFile++"\" u 1:"++show i++ " w l lt "++show lt++" lc "++show lc++" lw "++show lw
         offset   = 1
         sitData  = length ([(b,c) | b <- cars, c <- cars, b /= c] ++
                            [(b,c) | b <- cars, c <- cars, b < c])
         obsData  = sitData
         --diffData = obsData


delim :: String
delim = "\t"


interleave :: a -> [a] -> [a]
interleave _ []     = []
interleave _ [x]    = [x]
interleave y (x:xs) = x:y:interleave y xs

obsToJs :: O.Wrapper -> String
obsToJs ow' = "new Rstc(" ++ toJs ow' cars ++ ")"
   where toJs ow @ (O.Wrapper e) (b:c:ds) = "addM(" ++ toJs ow (c:ds) ++ ", " ++
                                                 show b ++ ", " ++ show c ++ ", " ++
                                                 "{ntg: " ++ show (O.ntg e b c :: Double) ++ ", " ++
                                                 " ttc: " ++ show (O.ttc e b c :: Double) ++ "})"
         toJs _                     _        = "{}"


printObsJs :: Sit (Prim (Qty Double)) -> [String]
printObsJs s = "var obs = [];" : map (\(i,oa) -> "obs[" ++ show i ++ "] = {time: " ++ show (t (w oa)) ++ ", rstc: " ++ obsToJs (w oa) ++ "};") (zip ([0..] :: [Int]) (filter isObsOrInit (sit2list s)))
   where isObsOrInit (Match _) = True
         isObsOrInit (Init _)  = True
         isObsOrInit _         = False
         t :: O.Wrapper -> Double
         t (O.Wrapper e)       = (unwrap $ O.time e - tOffset)
         tOffset :: Qty Double
         tOffset               = tOffset' (sit2list s)
         tOffset' (Init e : _) = O.time e
         tOffset' (_ : as)     = tOffset' as
         tOffset' []           = error "Main.printObsJs: empty situation?"
         w (Match e)           = O.wrap e
         w (Init e)            = O.wrap e
         w _                   = error "Main.printObsJs: no observation action"


printSitJs :: Sit (Prim (Qty Double)) -> [String]
printSitJs ss = initial (sit2list ss) ++ map toJs (filter hasEffect (sit2list ss))
   where initial (Init e : _)       = ["var modRstc = new ChangingRstc(" ++ obsToJs (O.wrap e) ++ ").wait(0);"
                                      ,"modRstc.forceReferenceCar(obsRstc.computeReferenceCar());"
                                      ,"var modLane = {};"
                                      ,concat (map (\b -> "modLane[" ++ show b ++ ".id] = " ++ (case O.lane e b of RightLane -> "0" ; LeftLane -> "1") ++ ";") cars)
                                      ,"var tDone = -1;"
                                      ,"var tWait = 0;"]
         initial (_ : as)           = initial as
         initial []                 = error "Main.printSitJs: empty situation?"
         hasEffect (Accel _ _)      = True
         hasEffect (LaneChange _ _) = True
         hasEffect (Wait _)         = True
         hasEffect _                = False
         toJs (Accel b q)           = "if (t > tWait && tDone < tWait) { tOffset += modRstc.time(); modRstc.accel(" ++ show b ++ ".id, " ++ show (unwrap q) ++ ").progress().wait(0); tDone = tWait; }"
         toJs (LaneChange b l)      = "if (t > tWait && tDone < tWait) { modLane[" ++ show b ++".id] = " ++ (case l of RightLane -> "0" ; LeftLane -> "1") ++ "; tDone = tWait; }"
         toJs (Wait t)              = "tWait += " ++ show (unwrap t) ++ ";"
         toJs _                     = error "Main.printSitJs: no effect action"


main :: IO ()
main = do
         -- _ <- forkServer (pack "localhost") 8000
{-
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
   mapM_ (\i -> putStrLn (show i ++ ": " ++ show (doo i p2 S0))) [0..9]
   putStrLn "-------------------------------------------------------"
   mapM_ (\i -> putStrLn ("Pick one: " ++ show i ++ ": " ++ show (doo i (p3 i) S0))) [0..5]
   putStrLn "-------------------------------------------------------"
   mapM_ (\i -> putStrLn ("Pick seq: " ++ show i ++ ": " ++ show (doo i (p4 i) S0))) [0..5]
   putStrLn "-------------------------------------------------------"
   mapM_ (\i -> putStrLn ("Pick nested: " ++ show i ++ ": " ++ show (doo i (p5 i) S0))) [0..5]
   putStrLn "-------------------------------------------------------"
   mapM_ (\i -> putStrLn ("Pick nested 2: " ++ show i ++ ": " ++ show (doo i (p6 i) S0))) [0..5]
   putStrLn "-------------------------------------------------------"
-}
         --mapM_ (putStrLn . show) (take 30 O.observations)
         --putStrLn "-------------------------------------------------------"
         --mapM_ (\i -> putStrLn ((show . cutoff i) (tree (obsprog O.observations) S0 0.0 0))) [0..30]
         --putStrLn "-------------------------------------------------------\n"
-- {-
         let obs      = take 100 O.observations
             obsProg  = obsprog obs
             --candProg = overtake H D
             candProg = pass D B `Conc` overtake H B
             prog     = obsProg `Conc` candProg
             --sits     = map (\(s,_,_) -> s) $ do3 4 prog s0
             sits     = map sit $ doo' (treeDT lookahead prog s0)
             --confs    = doo' (treeDT 6 prog s0)
             partOfObs (Wait _)     = True
             partOfObs (Prematch _) = True
             partOfObs (Match _)    = True
             partOfObs _            = False
             dropObs (Wait _ : Prematch _ : x @ (Match _) : xs) = x : dropObs xs
             dropObs (Wait _ :              x @ (Match _) : xs) = x : dropObs xs
             dropObs (                      x             : xs) = x : dropObs xs
             dropObs []                                         = []
             --newsit s q = inject 2 (Accel H q) (remove 2 s)
{-
             newquality s @ (Do (Prematch e) _) q    = quality (newsit s q) e H D
             newquality _                       _    = 100000
             revnewquality s @ (Do (Prematch e) _) q = quality (newsit s q) e D H
             revnewquality _                       _ = 100000
             ttcnewquality s @ (Do (Prematch e) _) q = ttcDiff (newsit s q) e D H
             ttcnewquality _                       _ = 100000
             newqualities s @ (Do (Prematch _) _)    = zip3 ticks (map (newquality s) ticks) (map (revnewquality s) ticks)
             newqualities _                          = []
             ticks = [-30.0, -29.9 .. 30.0]
             diffs xs = map (\(m,n) -> xs !! m - xs !! n) (zip [0 .. length xs - 2] [1 .. length xs - 1])
             s0 (Do (Prematch _) (Do (Wait _) (Do (Accel _ _) s))) = Just s
             s0 _                                                              = Nothing
             s1 (Do (Prematch _) (Do (Wait _) s)) = Just s
             s1 _                                         = Nothing
             s2 (Do (Prematch _) s) = Just s
             s2 _                       = Nothing
             posInf = 10000000
-}
             isMatch s = case history s of Match _ : _ -> True
                                           _           -> False
             isEnd s = case history s of End _ _ : _ -> True
                                         _           -> False
--         putStrLn (show (force (tree prog S0 0 0)))
         --mapM_ (\(s,v,d) ->
         mapM_ (\s ->
            do
               --let Conf t s = c
               --putStrLn (show (sit2list s))
               if isMatch s || isEnd s
                  then do  putStrLn (show (filter (not.partOfObs) (sit2list s)))
                           putStrLn (show (dropObs (sit2list s)))
                           putStrLn (show (sit2list s))
                           --putStrLn (show (v, d))
                           printFluents s
                           --putStrLn (show t)
{-
                           if False && 19.5 < time s && time s < 19.7
                              then do
                                       --mapM_ (putStrLn . show) (newqualities s)
                                       putStrLn ("ntg " ++ show (ntg s H D))
                                       putStrLn ("ttc " ++ show (ttc s H D))
                                       putStrLn ("ntg after accel " ++ show (ntg (Do (Accel H posInf) s) H D))
                                       putStrLn ("ttc after accel " ++ show (ttc (Do (Accel H posInf) s) H D))
                                       putStrLn ("ntg after wait " ++ show (ntg (Do (Wait 0.5) (Do (Accel H posInf) s)) H D))
                                       putStrLn ("ttc after wait " ++ show (ttc (Do (Wait 0.5) (Do (Accel H posInf) s)) H D))
                                       putStrLn ("rev ntg " ++ show (ntg s H D))
                                       putStrLn ("rev ttc " ++ show (ttc s H D))
                                       putStrLn ("rev ntg after accel " ++ show (ntg (Do (Accel H posInf) s) D H))
                                       putStrLn ("rev ttc after accel " ++ show (ttc (Do (Accel H posInf) s) D H))
                                       putStrLn ("rev ntg after wait " ++ show (ntg (Do (Wait 0.5) (Do (Accel H posInf) s)) D H))
                                       putStrLn ("rev ttc after wait " ++ show (ttc (Do (Wait 0.5) (Do (Accel H posInf) s)) D H))
                                       putStrLn "---"
                                       let f   = newquality s
                                       let cfl = canonicalize Linear f 0
                                       let cfr = canonicalize Recip f 0
                                       let g   = revnewquality s
                                       let cgl = canonicalize Linear g 0
                                       let cgr = canonicalize Recip g 0
                                       let h   = ttcnewquality s
                                       let chl = canonicalize Linear h 0
                                       let chr = canonicalize Recip h 0
                                       let na  = nullAt id
                                       let pick = head . sortBy (compare.abs) 
                                       --putStrLn ("nullAt f lin " ++ show (na cfl) ++ "  =>  " ++ show (map f (na cfl)))
                                       putStrLn ("nullAt f rec " ++ show (na cfr) ++ "  =>  " ++ show (map f (na cfr)))
                                       putStrLn ("nullAt g lin " ++ show (na cgl) ++ "  =>  " ++ show (map g (na cgl)))
                                       putStrLn ("nullAt h lin " ++ show (na chl) ++ "  =>  " ++ show (map g (na chl)))
                                       putStrLn ("nullAt h rec " ++ show (na chr) ++ "  =>  " ++ show (map g (na chr)))
                                       putStrLn ("#q,f,g,h,f+g,f+g+h,|f|+|g|,|f|+|g|")
                                       mapM_ (putStrLn . tail . init . show) (map (\q -> (q,f q, g q, h q, f q + g q, f q + g q + h q, abs (f q) + abs (g q), abs (f q) + abs (g q) + abs (h q))) ([pick (na cfr) - 1.0, pick (na cfr) - 0.9 .. pick (na cgl) + 1.0]))
                                       --putStrLn ("nullAt g rec " ++ show (na cgr) ++ "  =>  " ++ show (g (na cgr)))
                                       --mapM_ (putStrLn . show) (diffs (map snd (newqualities s)))
                                       --putStrLn (show t)
                                       --putStrLn "---"
                                       --putStrLn (show (dropObs (sit2list (newsit s xInterpolateRecipLinAndLin))))
                                       --putStrLn ("ntg0 = " ++ (show (maybe (-1000) (\sit -> ntg sit H D) (s0 (newsit s xInterpolateRecipLin)))))
                                       --putStrLn ("ttc0 = " ++ (show (maybe (-1000) (\sit -> ttc sit H D) (s0 (newsit s xInterpolateRecipLin)))))
                                       --putStrLn ("ntg1 = " ++ (show (maybe (-1000) (\sit -> ntg sit H D) (s1 (newsit s xInterpolateRecipLin)))))
                                       --putStrLn ("ttc1 = " ++ (show (maybe (-1000) (\sit -> ttc sit H D) (s1 (newsit s xInterpolateRecipLin)))))
                                       --putStrLn ("ntg2 = " ++ (show (maybe (-1000) (\sit -> ntg sit H D) (s2 (newsit s xInterpolateRecipLin)))))
                                       --putStrLn ("ttc2 = " ++ (show (maybe (-1000) (\sit -> ttc sit H D) (s2 (newsit s xInterpolateRecipLin)))))
                              else return ()
-}
                           if False && 19.5 <= time s && time s < 19.7 ||
                              False && 29.0 <= time s && time s < 29.5 ||
                              False && 29.5 <= time s ||
                              True && (case history s of End _ _ : _ -> True ; _ -> False)
                              then do writeFile "trace.csv" (unlines (traceCsv s))
                                      writeFile "plot.sh" (unlines (gnuplot "trace.csv"))
                                      mapM_ putStrLn (printObsJs s)
                                      mapM_ putStrLn (printSitJs s)
                              else return ()
                           putStrLn ""
                  else return ()
            ) sits
         putStrLn "-------------------------------------------------------"
         --let showTreeDepth = 20
         --let treeDepth = 4
         --putStrLn $ showTree showTreeDepth $ let Conf t _ = treeT treeDepth prog s0 in t
         --putStrLn "-------------------------------------------------------"
         --putStrLn $ showTree showTreeDepth $ let Conf t _ = treeDT treeDepth prog s0 in t
         --putStrLn "-------------------------------------------------------"
         stats <- getGCStats
         putStrLn "GC Stats:"
         putStrLn (show stats)
         putStrLn "-------------------------------------------------------"
         timeCost <- Util.MemoCache.timeCost
         ticksCost <- Util.MemoCache.ticksCost
         putStrLn ("Caching time cost: " ++ show timeCost ++ " s, " ++ show ticksCost ++ " ticks")
         putStrLn "-------------------------------------------------------"
-- -}
{-
         mapM_ (putStrLn . show) (map (maybe Nothing $ (\x -> Just
               ( x
               , O.time x
               , map (O.lane x) [B,D,H]
               , map (uncurry (O.ntg x)) [(x,y) | x <- [B,D,H], y <- [B,D,H]]
               , map (uncurry (O.ttc x)) [(x,y) | x <- [B,D,H], y <- [B,D,H]]
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
   where bla x @ (Just t') | depth t' >= 35 = Just (debug' ((show (value l t')) ++ " " ++ (show (depth t'))) t')
                           | otherwise = x
         bla x @ Nothing = x
-}
-}

