{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Blaze.ByteString.Builder (copyByteString)
import Data.ByteString.Char8 (pack)
import Data.List (isSuffixOf)
import Data.Monoid
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp

import RSTC.Car
import Interpreter.Golog
import RSTC.BAT.Progression
import qualified RSTC.Obs as Obs
import RSTC.Progs


type MyConf = (Sit (Prim Double), Reward, Depth)

pr :: [MyConf]
pr = let obs  = obsprog $ take 100 Obs.observations
         cand = pass D B `Conc` overtake H B
         prog = obs `Conc` cand
     in do3 lookahead prog s0


main :: IO ()
main = do
    let port = 8080
    putStrLn $ "Listening on port " ++ show port
    run port (app pr)


app :: Monad m => [MyConf] -> Request -> m Response
app confs req = do
    let paths = pathInfo req
    let path = case paths of p:_ -> (read $ show $ p) :: String ; _ -> "index.html"
    return $ if all (`elem` ['0'..'9']) path
             then index confs (read path :: Int)
             else indexFile path


indexFile :: String -> Response
indexFile fileName = ResponseFile status200 [("Content-Type", pack $ mimeType fileName)] ("html/" ++ fileName) Nothing


index :: [MyConf] -> Int -> Response
index confs i = ResponseBuilder status [("Content-Type", "text/plain")] response
   where restConfs = drop i confs
         (s,r,d)   = case restConfs of x:_ -> x ; _ -> undefined
         ntgs      = [(b, c, ntg s b c) | b <- cars, c <- cars, b /= c]
         ttcs      = [(b, c, ttc s b c) | b <- cars, c <- cars, b < c]
         lanes     = [(b, lane s b) | b <- cars]
         ontgs     = case history s of (Match e):_ -> [(b, c, Obs.ntg e b c) | b <- cars, c <- cars, b /= c] ; _ -> []
         ottcs     = case history s of (Match e):_ -> [(b, c, Obs.ttc e b c) | b <- cars, c <- cars, b /= c] ; _ -> []
         olanes    = case history s of (Match e):_ -> [(b, Obs.lane e b) | b <- cars] ; _ -> []
         isMatch   = case history s of (Match _):_ -> True ; _ -> False
         status    = case restConfs of _:_ -> status200
                                       _   -> status404
         response  = case restConfs of _:_ -> mconcat $ map copyByteString $ json
                                       _   -> copyByteString "{}"
         tToStr    = \(b,c,t) -> ("{\"b\":\"" ++ show b ++ "\", \"c\":\"" ++ show c ++"\", \"t\":" ++ show t ++ "} ")
         lToStr    = \(b,l) -> ("{\"b\":\"" ++ show b ++ "\", \"l\":" ++ show (laneToNumber l) ++ "} ")
         json      =
            [ "{ \"isMatch\": ", if isMatch then "true" else "false", "\n" ] ++
            [ ", \"action\": ", (case history s of a:_ -> pack (toJson a) ; _ -> "null"), "\n" ] ++
            [ ", \"time\": ", pack $ show (time s), "\n" ] ++
            [ ", \"reward\": ", pack $ show r, "\n" ] ++
            [ ", \"depth\": ", pack $ show d, "\n" ] ++
            [ ", \"ntg\": [", pack (concat $ interleave ", " $ map tToStr ntgs), "]\n"] ++
            [ ", \"ttc\": [", pack (concat $ interleave ", " $ map tToStr ttcs), "]\n"] ++
            [ ", \"lane\": [", pack (concat $ interleave ", " $ map lToStr lanes), "]\n"] ++
            [ ", \"ontg\": [", pack (concat $ interleave ", " $ map tToStr ontgs), "]\n"] ++
            [ ", \"ottc\": [", pack (concat $ interleave ", " $ map tToStr ottcs), "]\n"] ++
            [ ", \"olane\": [", pack (concat $ interleave ", " $ map lToStr olanes), "]\n"] ++
            [ "}" ]


mimeType :: String -> String
mimeType f = helper m
   where m = [ ("svg", "image/svg+xml")
             , ("html", "text/html")
             , ("js", "application/javascript")
             , ("css", "text/css")
             , ("ttf", "application/x-font-ttf")
             ]
         helper ((ending,mime):xs) | isSuffixOf ending f = mime
                                   | otherwise           = helper xs
         helper [] = "text/plain"


toJson :: Show a => Prim a -> String
toJson (Wait t) = "{ \"wait\": { \"t\": " ++ (show t) ++ "} }"
toJson (Accel b q) = "{ \"accel\": { \"b\": \"" ++ (show b) ++ "\", \"q\": " ++ (show q) ++ "} }"
toJson (LaneChange b l) = "{ \"lc\": { \"b\": \"" ++ (show b) ++ "\", \"l\": " ++ (show $ laneToNumber l) ++ "} }"
toJson (Init e) = "{ \"init\": { \"time\": " ++ (show (Obs.time e)) ++ " } }"
toJson (Prematch e) = "{ \"prematch\": { \"time\": " ++ (show (Obs.time e)) ++ " } }"
toJson (Match e) = "{ \"match\": { \"time\": " ++ (show (Obs.time e)) ++ " } }"
toJson Abort = "{ \"abort\": {} }"
toJson NoOp = "{ \"noop\": {} }"
toJson (Start b s) = "{ \"start\": { \"b\": \"" ++ (show b) ++ "\", \"prog\": \"" ++ s  ++ "\" } }"
toJson (End b s) = "{ \"end\": { \"b\": \"" ++ (show b) ++ "\", \"prog\": \"" ++ s ++ "\" } }"
toJson (Msg s) = "{ \"msg\": { \"msg\": \"" ++ s ++ "\" } }"


laneToNumber :: Lane -> Double
laneToNumber RightLane = 0
laneToNumber LeftLane  = 1


interleave :: a -> [a] -> [a]
interleave _ []     = []
interleave y (x:xs) = x : concat (map (\z -> [y,z]) xs)

