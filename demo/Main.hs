{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative

import RoboMonad

import Maths
import PidController

data MyRobo =
     MyRobo { _testPid :: PidController Scalar Scalar
            , _target  :: Angle
            , _ticks   :: Int
            }

makeLenses ''MyRobo

myRobo :: MyRobo
myRobo = MyRobo { _testPid = makePidSimple 50 0 30
                , _target  = 0
                , _ticks   = 0
                }

initBot :: Robo MyRobo ()
initBot = do
  setGunSpeed 2
  setThrust 250

run :: Robo MyRobo ()
run = do
  ticks += 1

  nt <- use ticks
  if nt `mod` 20 == 0
     then target += 4 * pi / 3
     else return ()

  -- pid controller towards target angle
  ang <- getHeading
  tAng <- use target
  testPid %= updatePid (normaliseAngle (tAng - ang))
  out <- use $ testPid.pidOut
  setTurnPower out

  setFiring 1

scan :: ScanData -> Robo MyRobo ()
scan e = return ()

testBot :: BotSpec
testBot = BotSpec { botName = "test"
                  , botInitialState = myRobo
                  , onInit = initBot
                  , onTick = run
                  , onScan = const (return ()) }

myRules :: BattleRules
myRules = defaultRules

main :: IO ()
main = runWorld myRules [testBot]
