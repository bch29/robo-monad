{-# LANGUAGE TemplateHaskell #-}
module TestBot (testbot) where

import Game.Robo
import Game.Robo.Maths
import Game.Robo.PidController

type TestBot = Robo TestBotState

data TestBotState = TestBotState
  { _testPid :: PidController Scalar Scalar
  , _gunPid  :: PidController Scalar Scalar
  , _target  :: Angle
  , _ticks   :: Int
  , _enemyPos :: Maybe Vec
  }

makeLenses ''TestBotState

myInitialState :: TestBotState
myInitialState = TestBotState
  { _testPid = makePidSimple 50 0 30
  , _gunPid  = makePid 15 1 1 0.5
  , _target  = 0
  , _ticks   = 0
  , _enemyPos = Nothing
  }

initBot :: TestBot ()
initBot = do
  setGunSpeed 2
  setThrust 250
  setRadarSpeed 16

run :: TestBot ()
run = do
  ticks += 1
  ticks %= (`mod` 20)

  nt <- use ticks
  if nt == 0
     then target += 4 * pi / 3
     else return ()

  -- pid controller towards target angle
  do ang <- getHeading
     tAng <- use target
     testPid %= updatePid (angNormRelative (tAng - ang))
     setTurnPower =<< use (testPid.pidOut)

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
      setGunSpeed =<< use (gunPid.pidOut)
    Nothing -> return ()

scan :: ScanData -> TestBot ()
scan (ScanData distance angle) = do
  pos <- getPosition
  enemyPos .= Just (pos + (vecFromAngle angle |* distance))
  setFiring 2

testbot :: BotSpec
testbot = BotSpec
  { botName = "testbot"
  , botInitialState = myInitialState
  , onInit = initBot
  , onTick = run
  , onScan = scan
  }
