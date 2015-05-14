{-|
Module      : BulletTester
Description : A robot designed to act as a stress tester for bullets.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable (depends on SDL)

Attempts to fire in a direction where it can't see any other robots.
-}

{-# LANGUAGE TemplateHaskell #-}

module BulletTester (bullettester) where

-- Import of Game.Robo required, Maths and PID
-- provide handy utility functions.
import Game.Robo
import Game.Robo.Maths
import Game.Robo.Extra
import Game.Robo.PID.Lensed

import Data.List
import Data.Ord

-- | Provides a handy alias for this robot's Robo monad.
type BulletTester = Robo BulletTesterState

-- | Put all of the variables that you need here, prefixed by
-- underscores (so that template haskell can convert them to
-- lenses).
data BulletTesterState = BulletTesterState
  { _gunPid :: PID Scalar Scalar
  , _turnPid :: PID Scalar Scalar
  , _turnTarget :: Angle
  , _gunTarget :: Angle
  }

-- | Give fields their initial values.
emptyState :: BulletTesterState
emptyState = BulletTesterState
  { _gunPid = makePidSimple 80 20 40
  , _turnPid = makePidSimple 80 20 40
  , _gunTarget = 0
  , _turnTarget = 0
  }

-- This converts fields in BulletTesterState to lenses which can be used
-- to easily access variables.
makeLenses ''BulletTesterState

findAngle :: BulletTester Angle
findAngle = do
  pos <- getPosition
  centre <- (|* 0.5) <$> getRule ruleArenaSize
  return (centre `angleTo` pos)

-- | Runs when the bot is first created.
myInit :: BulletTester ()
myInit = do
  ang <- findAngle
  turnTarget .= ang
  setThrust 100
  return ()

tickGun :: BulletTester ()
tickGun = do
  -- aim for the centre
  size <- getRule ruleArenaSize
  pos <- getPosition
  gunTarget .= angleToHorizontal (size |* 0.5 - pos)

  -- do the PID stuff
  realAng <- getGunAbsHeading
  target <- use gunTarget
  gunPid %= updatePid (angNormRelative $ target - realAng)
  setGunTurnPower =<< use (gunPid.pidOut)
  setFiring 0.5

tickTurning :: BulletTester ()
tickTurning = do
  ang <- getHeading
  target <- use turnTarget
  turnPid %= updatePid (angNormRelative $ target - ang)
  setTurnPower =<< use (turnPid.pidOut)

-- | Runs every game tick.
myTick :: BulletTester ()
myTick = do
  tickGun
  tickTurning

-- | Runs when the radar passes over an enemy robot.
myScan :: ScanData -> BulletTester ()
myScan s = do
  return ()

-- | Runs when the robot is hit by an enemy bullet.
myOnHitByBullet :: BulletTester ()
myOnHitByBullet = do
  return ()

-- | Runs when a bullet fired by this robot hits an enemy.
myOnBulletHit :: BulletTester ()
myOnBulletHit = do
  return ()

-- | Runs when this robot collides with the arena walls.
myOnCollideWall :: WallCollisionData -> BulletTester ()
myOnCollideWall w = do
  return ()

-- | This is the actual robot specification to be passed to
-- @runWorld@.
bullettester :: BotSpec
bullettester = BotSpec
  { botName         = "bullettester"
  , botInitialState = emptyState
  , onInit          = myInit
  , onTick          = myTick
  , onScan          = myScan
  , onHitByBullet   = myOnHitByBullet
  , onBulletHit     = myOnBulletHit
  , onCollideWall   = myOnCollideWall
  }
