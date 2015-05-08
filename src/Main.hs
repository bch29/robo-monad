{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad.State

import Lens.Family2
import Lens.Family2.TH

import RoboMonad
import Core
import World

import Maths
import PidController

data MyRobo =
     MyRobo { _testPid :: PidController Scalar Scalar }

myRobo :: MyRobo
myRobo = MyRobo { _testPid = makePid 5 1 4 }

initBot :: Robo MyRobo ()
initBot = do
  setGunSpeed 1
  setThrust (-500)
  setTurnPower 16

run :: Robo MyRobo ()
run = do
  ang <- getHeading
  setFiring 1

scan :: ScanData -> Robo MyRobo ()
scan e = return ()

testBot :: BotSpec
testBot = BotSpec { botName = "test"
                  , botInitialState = myRobo
                  , onInit = initBot
                  , onTick = run
                  , onScan = const (return ()) }

main :: IO ()
main = runWorld defaultRules [testBot]
