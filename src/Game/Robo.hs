{-|
Module      : Game.Robo
Description : The main module that needs to be imported in order to define robots.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE Trustworthy #-} -- Enables compilation of robot files with Safe Haskell
{-# LANGUAGE RankNTypes  #-} -- Required for lens function type signatures

module Game.Robo
  (
  -- * Core
    runWorld
  , Robo
  , BotSpec (..)
  , BotSpec'
  , spec

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

import Lens.Micro.Platform as Lens

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

-- | Wrap a 'BotSpec' so it can be passed to 'runWorld'.
spec :: BotSpec s -> BotSpec'
spec = BotSpec'

---------------------------------
--  ACCESSORS
---------------------------------

-- | Get the rules object.
rules :: Robo s Rules
rules = (Robo . liftF . RulesR) id

-- | Get the value of a rule from its lens.
getRule :: Getter Rules a -> Robo s a
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
  theBotHeading <- getHeading
  theGunHeading <- getGunRelHeading
  return (theBotHeading + theGunHeading)

-- | Get the direction in which the robot's radar is facing relative to the robot.
getRadarRelHeading :: Robo s Angle
getRadarRelHeading = useI (botRadar.radHeading)

-- | Get the absolute direction in which the robot's radar is facing.
getRadarAbsHeading :: Robo s Angle
getRadarAbsHeading = do
  theBotHeading <- getHeading
  radarHeading <- getRadarRelHeading
  return (theBotHeading + radarHeading)

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
  minP <- getRule ruleMinFirePower
  maxP <- getRule ruleMaxFirePower
  case () of
    _ | power < 0                 -> setIL (botGun.gunFiring) 0
      | power > 0 && power < minP -> setIL (botGun.gunFiring) minP
      | power > maxP              -> setIL (botGun.gunFiring) maxP
      | otherwise                 -> setIL (botGun.gunFiring) power

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
  :: (Num b, Ord b) =>
     Getter Rules b
     -> ASetter BotState BotState a b -> b -> Robo s ()
setICapped capBy setter val = do
  limit <- fmap (view capBy) rules
  case () of
    () | val > limit  -> setIL setter limit
       | val < -limit -> setIL setter (-limit)
       | otherwise    -> setIL setter val

getI :: Robo s BotState
getI = (Robo . liftF . GetIStateR) id

setI :: BotState -> Robo s ()
setI newState = (Robo . liftF) (PutIStateR newState ())

useI :: Getter BotState b -> Robo s b
useI l = fmap (view l) getI

setIL :: ASetter BotState BotState a b -> b -> Robo s ()
setIL setter a = overI (set setter a)

overI :: (BotState -> BotState) -> Robo s ()
overI f = getI >>= setI . f
