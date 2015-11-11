{-|
Module      : WallHugger
Description : A robot that sticks to the walls and fires at a target.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE UnicodeSyntax #-}

module WallHugger (wallhugger) where

import           Control.Monad

import           Game.Robo
import           Game.Robo.Extra
import           Game.Robo.Maths
import           Game.Robo.PID.Lensed

import           Data.Maybe

type WallHugger = Robo WallHuggerState

data WallHuggerState = WallHuggerState
  { _gunPid    :: PID Double Double
  , _enemyPos  :: Maybe Vec
  , _direction :: Scalar
  }

gunPid ∷ Lens' WallHuggerState (PID Scalar Scalar)
gunPid f s = fmap (\x -> s { _gunPid = x }) (f (_gunPid s))

direction ∷ Lens' WallHuggerState Scalar
direction f s = fmap (\x -> s { _direction = x }) (f (_direction s))

enemyPos ∷ Lens' WallHuggerState (Maybe Vec)
enemyPos f s = fmap (\x -> s { _enemyPos = x }) (f (_enemyPos s))

emptyState ∷ WallHuggerState
emptyState = WallHuggerState
  { _gunPid    = makePidSimple 150 0 80
  , _enemyPos  = Nothing
  , _direction = 1
  }

myInit ∷ WallHugger ()
myInit = do
  setThrust 1000
  setRadarSpeed 16

adjustGun ∷ WallHugger ()
adjustGun = do
  Just enemy <- use enemyPos
  ourPos     <- getPosition
  gunAngle   <- getGunAbsHeading
  let offAngle   = ourPos `angleTo` enemy
      errorAngle = angNormRelative (offAngle - gunAngle)
  gunPid %= updatePid errorAngle
  out <- use (gunPid.pidOut)
  setGunTurnPower out

adjustMotion ∷ WallHugger ()
adjustMotion = do
  hv       <- getHeadingVec
  dist     <- getWallDistR
  perpDist <- getWallDistDirR (vecPerpR hv)
  backDist <- getWallDistDirR (-hv)
  let perpTp
        | perpDist < 100 = 10
        | perpDist < 200 = -16
        | otherwise = 0
      normTp | dist < 200 = 32
             | otherwise = 0
  setTurnPower (perpTp + normTp)
  if dist < 100 && dist < backDist
     then direction .= -1
     else direction .= 1
  setThrust =<< (*) 1000 <$> use direction

myTick ∷ WallHugger ()
myTick = do
  adjustMotion
  enemy <- use enemyPos
  when (isJust enemy) adjustGun
  setFiring 2

myScan ∷ ScanData → WallHugger ()
myScan (ScanData dist ang) = do
  pos <- getPosition
  enemyPos .= (Just $ vec (dist * cos ang) (dist * sin ang) + pos)
  return ()

myOnHitByBullet ∷ WallHugger ()
myOnHitByBullet = return ()

myOnBulletHit ∷ WallHugger ()
myOnBulletHit = return ()

myOnCollideWall ∷ WallCollisionData → WallHugger ()
myOnCollideWall _ = return ()

wallhugger ∷ BotSpec WallHuggerState
wallhugger = BotSpec
  { botName         = "wallhugger"
  , botInitialState = emptyState
  , onInit          = myInit
  , onTick          = myTick
  , onScan          = myScan
  , onHitByBullet   = myOnHitByBullet
  , onBulletHit     = myOnBulletHit
  , onCollideWall   = myOnCollideWall
  }
