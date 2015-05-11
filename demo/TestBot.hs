{-# LANGUAGE TemplateHaskell #-}
module TestBot (testbot) where

import Game.Robo
import Game.Robo.Maths
import Game.Robo.PidController

import Control.Monad

type TestBot = Robo TestBotState

data TestBotState = TestBotState
  { _turningPid  :: PidController Scalar Scalar
  , _gunPid      :: PidController Scalar Scalar
  , _target      :: Vec
  , _targetAngle :: Angle
  , _ticks       :: Int
  , _enemyPos    :: Maybe Vec
  }

makeLenses ''TestBotState

myInitialState :: TestBotState
myInitialState = TestBotState
  { _turningPid  = makePidSimple 50 0 30
  -- PID controller with no D gain used for spray effect
  , _gunPid      = makePidSimple 200 0 0
  , _target      = vec 400 400
  , _targetAngle = 0
  , _ticks       = 0
  , _enemyPos    = Nothing
  }

initBot :: TestBot ()
initBot = do
  setThrust 250
  setRadarSpeed 16

run :: TestBot ()
run = do
  ticks += 1
  ticks %= (`mod` 20)

  nt <- use ticks
  when (nt == 0) $ do
    targetAngle += pi / 3
    tang <- use targetAngle
    target .= vec 400 400 + 200 *| vecFromAngle tang

  -- pid controller towards target position
  do pos  <- getPosition
     targ <- use target
     ang  <- getHeading
     let tAng = pos `angleTo` targ
     turningPid %= updatePid (angNormRelative (tAng - ang))
     setTurnPower =<< use (turningPid.pidOut)

  -- gun pid controller
  mep <- use enemyPos
  case mep of
    Just ep -> do
      pos <- getPosition
      ang <- getGunAbsHeading
      let dist = vecMag (pos - ep)
          correction = (dist / 800) * (7*pi/24)
          tAng = pos `angleTo` ep + correction
      gunPid %= updatePid (angNormRelative (tAng - ang))
      setGunTurnPower =<< use (gunPid.pidOut)
      setFiring 0.5
    Nothing -> return ()

scan :: ScanData -> TestBot ()
scan (ScanData distance angle) = do
  pos <- getPosition
  enemyPos .= Just (pos + (vecFromAngle angle |* distance))

myOnHitByBullet :: TestBot ()
myOnHitByBullet = do
  return ()

myOnBulletHit :: TestBot ()
myOnBulletHit = do
  return ()

testbot :: BotSpec
testbot = BotSpec
  { botName         = "testbot"
  , botInitialState = myInitialState
  , onInit          = initBot
  , onTick          = run
  , onScan          = scan
  , onHitByBullet   = myOnHitByBullet
  , onBulletHit     = myOnBulletHit
  }
