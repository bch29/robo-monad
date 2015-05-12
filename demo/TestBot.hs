{-# LANGUAGE TemplateHaskell #-}
module TestBot (testbot) where

import Game.Robo
import Game.Robo.Maths
import Game.Robo.PidController

import Control.Monad
import Control.Applicative

type TestBot = Robo TestBotState

data TestBotState = TestBotState
  { _turningPid  :: PidController Scalar Scalar
  , _direction   :: Scalar
  , _targetAngle :: Angle
  }

makeLenses ''TestBotState

myInitialState :: TestBotState
myInitialState = TestBotState
  { _turningPid  = makePidSimple 50 0 30
  -- PID controller with no D gain used for spray effect
  , _direction   = 1
  , _targetAngle = 0
  }

initBot :: TestBot ()
initBot = do
  setThrust 500
  ang <- getRandomR (-pi, pi)
  -- set off in a random direction
  targetAngle .= ang

run :: TestBot ()
run = do
  -- pid controller towards target angle
  do ang  <- getHeading
     tAng <- use targetAngle
     turningPid %= updatePid (angNormRelative (tAng - ang))
     setTurnPower =<< use (turningPid.pidOut)

scan :: ScanData -> TestBot ()
scan (ScanData distance angle) = do
  -- fire a bullet when we see an enemy
  setFiring 4

myOnHitByBullet :: TestBot ()
myOnHitByBullet = do
  return ()

myOnBulletHit :: TestBot ()
myOnBulletHit = do
  return ()

chooseAngle :: Angle -> TestBot ()
chooseAngle hitAngle = do
  heading <- getHeading
  dir <- use direction
  let absAng = angNormAbsolute $ heading + hitAngle - pi/2
      -- the new angle should be somewhat close to the opposite direction
      -- to the angle made with the normal to the wall, so that we drive
      -- away from the wall instead of back towards it
      range = case () of
                _ | absAng < pi/2   -> (-pi, -pi/2)
                  | absAng < pi     -> (-pi/2, 0)
                  | absAng < 3*pi/2 -> (0, pi/2)
                  | otherwise       -> (pi/2, pi)
      -- the angle is in the opposite direction if we are driving backwards
      range' = if dir < 0
                  then case range of (a, b) -> (a+pi, b+pi)
                  else range
  tang <- getRandomR range'
  targetAngle .= tang

myOnCollideWall :: WallCollisionData -> TestBot ()
myOnCollideWall dat = do
  direction *= -1
  let a1 = abs (wcolAngle dat)
      a2 = abs (angNormRelative $ wcolAngle dat - pi)
  -- only choose a new angle when we aren't too close
  -- to parallel (or anti-parallel) to the wall
  when (a1 > 0.3 && a2 > 0.3) (chooseAngle (wcolAngle dat))
  setThrust =<< (*) 500 <$> use direction

testbot :: BotSpec
testbot = BotSpec
  { botName         = "testbot"
  , botInitialState = myInitialState
  , onInit          = initBot
  , onTick          = run
  , onScan          = scan
  , onHitByBullet   = myOnHitByBullet
  , onBulletHit     = myOnBulletHit
  , onCollideWall   = myOnCollideWall
  }
