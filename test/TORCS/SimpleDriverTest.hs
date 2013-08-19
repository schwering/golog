{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TORCS.SimpleDriverTest where

import Data.List (sort)
import Data.Maybe (fromJust)
import Golog.Interpreter
import Golog.Macro
import Golog.Util
import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Debug.Trace

state = CarState {angle = 0.110275, curLapTime = 2.6, damage = 0.0, distFromStart = 2039.74, distRaced = 7.17822, TORCS.CarState.focus = [-1.0,-1.0,-1.0,-1.0,-1.0], fuel = 93.9909, TORCS.CarState.gear = 1, lastLapTime = 0.0, opponents = [200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0,200.0], racePos = 1, rpm = 2641.56, speedX = 6.057555555555556, speedY = 1.1177055555555554e-3, speedZ = -2.3819194444444445e-3, track = [9.64252,9.68843,10.3513,11.8335,14.6645,17.8626,20.0802,22.9366,26.9659,33.0175,42.9929,62.2845,114.563,200.0,20.3664,9.33624,6.87771,5.84484,5.44473], trackPos = 0.278829, wheelSpinVel = [19.6888,19.1289,19.6145,19.6145], z = 0.346978}


runTests :: IO Bool
runTests = $quickCheckAll

