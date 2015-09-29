{-|
Module      : Game.Robo
Description : The main module you need to import to make your own robots.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE Trustworthy #-} -- Enables compilation of robot files with Safe Haskell
{-# LANGUAGE RankNTypes  #-} -- Required for lens function type signatures
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- For deriving Robo instances

module Game.Robo
  (
  -- * Core
    runWorld
  , Robo
  , BotSpec (..)

  -- * Accessors
  , getRule
  , getPosition, getHeading, getHeadingVec, getSpeed, getAngVel
  , getGunRelHeading, getGunAbsHeading
  , getRadarRelHeading, getRadarAbsHeading
  , getEnergy

  -- * Control
  , setThrust, setTurnPower
  , setGunTurnPower, setFiring
  , setRadarSpeed

  -- * Printing
  , logLine, logShow

  -- * Rules
  , Rules (..)

  -- * Event data
  , ScanData (..), WallCollisionData (..)

  -- try to put all the lens stuff, etc, at the end
  -- so it doesn't get in the way of documentation
  -- * Basic maths stuff and lenses
  , module MTypes

  -- * Rule lenses
  , module Rules

  -- * Misc
  , module Misc
  , module Lens
  ) where

import Lens.Family2       as Lens
import Lens.Family2.TH    as Lens
import Lens.Family2.State as Lens

import Control.Monad.Free
import Control.Monad.State

import Control.Monad.Reader.Class as Misc
import Control.Monad.State.Class  as Misc
import Control.Monad.Random.Class as Misc

import Game.Robo.Maths
import Game.Robo.Core.Types
import Game.Robo.Core.Lenses
import Game.Robo.Core.World
import Game.Robo.Core.Rules       as Rules
import Game.Robo.Core.Types.Maths as MTypes

---------------------------------
--  ACCESSORS
---------------------------------

-- | Get the rules object.
rules :: Robo s Rules
rules = (Robo . liftF . RulesR) id

-- | Get the value of a rule from its lens.
getRule :: Getter' Rules a -> Robo s a
getRule rule = fmap (view rule) rules

-- | Get the robot's current position in pixels relative to the bottom-left corner.
getPosition :: Robo s Vec
getPosition = useI botPos

-- | Get the direction in which the robot is facing.
getHeading :: Robo s Angle
getHeading = useI botHeading

-- | Get the direction in which the robot is facing as a normalised vector.
getHeadingVec :: Robo s Vec
getHeadingVec = angleAsVec getHeading

-- | Get the robot's current forward (or backward if negative) speed.
getSpeed :: Robo s Scalar
getSpeed = useI botSpeed

-- | Get the robot's current angular velocity.
getAngVel :: Robo s Scalar
getAngVel = useI botAngVel

-- | Get the direction in which the robot's gun is facing relative to the robot.
getGunRelHeading :: Robo s Angle
getGunRelHeading = useI (botGun.gunHeading)

-- | Get the absolute direction in which the robot's gun is facing.
getGunAbsHeading :: Robo s Angle
getGunAbsHeading = do
  botHeading <- getHeading
  gunHeading <- getGunRelHeading
  return (botHeading + gunHeading)

-- | Get the direction in which the robot's radar is facing relative to the robot.
getRadarRelHeading :: Robo s Angle
getRadarRelHeading = useI (botRadar.radHeading)

-- | Get the absolute direction in which the robot's radar is facing.
getRadarAbsHeading :: Robo s Angle
getRadarAbsHeading = do
  botHeading <- getHeading
  radarHeading <- getRadarRelHeading
  return (botHeading + radarHeading)

-- | Get the robot's current available energy. See '_ruleMaxEnergy' and
-- '_ruleEnergyRechargeRate'.
getEnergy :: Robo s Scalar
getEnergy = useI botEnergy

-- | Utility function to convert an angle accessor to one that returns a vector
-- in the direction of the angle instead.
angleAsVec :: Robo s Angle -> Robo s Vec
angleAsVec = fmap vecFromAngle

---------------------------------
--  CONTROL
---------------------------------

-- | Set the engine thrust, which controls the robot's forward (or backward if negative)
-- acceleration.  Limited by game rules.
setThrust :: Scalar -> Robo s ()
setThrust = setICapped ruleMaxThrust botThrust

-- | Set the turning power in the clockwise direction (or anticlockwise if negative)
-- Limited by game rules.
setTurnPower :: Scalar -> Robo s ()
setTurnPower =
  setICapped ruleMaxAngThrust botAngThrust

-- | Set the turning power of the gun (positive is clockwise).
setGunTurnPower :: Scalar -> Robo s ()
setGunTurnPower =
  setICapped ruleMaxGunTurnPower
             (botGun . gunAngAcc)

-- | Set the firing power of the gun, where @0@ means don't fire. Limited by game rules.
setFiring :: Scalar -> Robo s ()
setFiring power = do
  min <- getRule ruleMinFirePower
  max <- getRule ruleMaxFirePower
  case () of
    _ | power < 0                -> setIL (botGun.gunFiring) 0
      | power > 0 && power < min -> setIL (botGun.gunFiring) min
      | power > max              -> setIL (botGun.gunFiring) max
      | otherwise                -> setIL (botGun.gunFiring) power

-- | Set the rotation speed of the radar (positive is clockwise).
setRadarSpeed :: Scalar -> Robo s ()
setRadarSpeed =
  setICapped ruleMaxRadSpeed
             (botRadar . radAngVel)

---------------------------------
--  LOGGING
---------------------------------

-- | Log something to the robot's console.
logMessage :: String -> Robo s ()
logMessage message = Robo (liftF (LogR message ()))

-- | Log a line to the robot's console.
logLine :: String -> Robo s ()
logLine message = do
  logMessage message
  logMessage "\n"

-- | Show something log it as a line to the robot's console.
logShow :: Show a => a -> Robo s ()
logShow = logLine . show

---------------------------------
--  UTILITY FUNCTIONS
---------------------------------

setICapped
  :: (Num a,Ord a)
  => Getter' Rules a -> Setter' BotState a -> a -> Robo s ()
setICapped capBy setter val = do
  limit <- fmap (view capBy) rules
  case () of
    () | val > limit  -> setIL setter limit
       | val < -limit -> setIL setter (-limit)
       | otherwise    -> setIL setter (val)

getI :: Robo s BotState
getI = (Robo . liftF . GetIStateR) id

setI :: BotState -> Robo s ()
setI newState = (Robo . liftF) (PutIStateR newState ())

useI :: FoldLike a BotState a' a b' -> Robo s a
useI l = fmap (view l) getI

setIL :: Setter' BotState a -> a -> Robo s ()
setIL setter a = overI (set setter a)

overI :: (BotState -> BotState) -> Robo s ()
overI f = getI >>= setI . f
