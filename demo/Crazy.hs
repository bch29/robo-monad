{-# LANGUAGE TemplateHaskell #-}
module Crazy (crazy) where

import Control.Applicative
import Control.Monad

import Game.Robo
import Game.Robo.Maths
import Game.Robo.PidController
import Game.Robo.Extra

type Crazy = Robo CrazyState

data CrazyState = CrazyState
     { _gunPid   :: PidController Double Double
     }

makeLenses ''CrazyState

emptyState :: CrazyState
emptyState = CrazyState
  { _gunPid   = makePidSimple 200 0 100
  }

myInit :: Crazy ()
myInit = do
  setThrust 1000

adjustGun :: Crazy ()
adjustGun = do
  arenaSize <- getRule ruleArenaSize
  ourPos    <- getPosition
  gunAngle  <- getGunAbsHeading
  let targetPos  = arenaSize |* 0.5
      offAngle   = ourPos `angleTo` targetPos
      errorAngle = angNormRelative (offAngle - gunAngle)
  gunPid %= updatePid errorAngle
  out <- use (gunPid.pidOut)
  setGunTurnPower out

myTick :: Crazy ()
myTick = do
  pos      <- getPosition
  hv       <- getHeadingVec
  dist     <- getWallDistR
  perpDist <- getWallDistDirR (vecPerpR hv)
  let perpTp = if perpDist < 100 then 10 else if perpDist < 200 then -16 else 0
      normTp = if dist < 200 then 32 else 0
  setTurnPower (perpTp + normTp)
  adjustGun
  setFiring 2

myScan :: ScanData -> Crazy ()
myScan s = do
  return ()

myOnHitByBullet :: Crazy ()
myOnHitByBullet = do
  return ()

myOnBulletHit :: Crazy ()
myOnBulletHit = do
  return ()

crazy :: BotSpec
crazy = BotSpec
  { botName         = "crazy"
  , botInitialState = emptyState
  , onInit          = myInit
  , onTick          = myTick
  , onScan          = myScan
  , onHitByBullet   = myOnHitByBullet
  , onBulletHit     = myOnBulletHit
  }
