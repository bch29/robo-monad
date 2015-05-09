{-# LANGUAGE RankNTypes #-}
module Game.Robo
  -- Re-exported
  ( BattleRules (..), ruleMaxThrust, ruleMaxAngThrust, ruleMaxGunSpeed, ruleMaxFirePower, ruleMinFirePower
                    , ruleMass, ruleDriveFriction, ruleTurnFriction, ruleBotSize, ruleGunSize
                    , ruleBulletSpeed, ruleArenaSize, ruleTickTime
  , BotSpec (..)
  , ScanData (..)
  , Robo
  , vec, rect
  , defaultRules

  , runWorld

  , getRandom, getRandomR

  , module Data.Vector.Class

  , module Lens.Family2
  , module Lens.Family2.State
  , module Lens.Family2.TH

  , module Control.Monad.State.Class
  , module Control.Monad.Reader.Class

  -- types
  , Vec, Scalar, Angle
  -- accessors
  , getRule
  , getPosition, getHeading, getHeadingVec, getSpeed, getAngVel
  , getGunRelHeading, getGunAbsHeading
  , getRadarRelHeading, getRadarAbsHeading
  -- control
  , setThrust, setTurnPower
  , setGunSpeed, setFiring
  , setRadarSpeed
  -- logging
  , printLine, printShow
  )
    where

import Lens.Family2
import Lens.Family2.TH
import Lens.Family2.State

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Random

import Control.Monad.Reader.Class
import Control.Monad.State.Class

import Data.Vector.Class hiding (Scalar)

import Game.Robo.Maths
import Game.Robo.Core
import Game.Robo.Core.World

---------------------------------
--  TYPES
---------------------------------

type Scalar = Double

---------------------------------
--  ACCESSORS
---------------------------------

-- | Get the value of a rule.
getRule :: Getter' BattleRules a -> Robo s a
getRule rule = withBot $ asks (view rule)

-- | Get the robot's current position in pixels relative to the bottom-left corner.
getPosition :: Robo s Vec
getPosition = useBot botPos

-- | Get the direction in which the robot is facing.
getHeading :: Robo s Angle
getHeading = useBot botHeading

-- | Get the direction in which the robot is facing as a normalised vector.
getHeadingVec :: Robo s Vec
getHeadingVec = angleAsVec getHeading

-- | Get the robot's current forward (or backward if negative) speed.
getSpeed :: Robo s Scalar
getSpeed = useBot botSpeed

-- | Get the robot's current angular velocity.
getAngVel :: Robo s Scalar
getAngVel = useBot botAngVel

-- | Get the direction in which the robot's gun is facing relative to the robot.
getGunRelHeading :: Robo s Angle
getGunRelHeading = useBot (botGun.gunHeading)

-- | Get the absolute direction in which the robot's gun is facing.
getGunAbsHeading :: Robo s Angle
getGunAbsHeading = do
  botHeading <- getHeading
  gunHeading <- getGunRelHeading
  return $ botHeading + gunHeading

-- | Get the direction in which the robot's radar is facing relative to the robot.
getRadarRelHeading :: Robo s Angle
getRadarRelHeading = useBot (botRadar.radHeading)

-- | Get the absolute direction in which the robot's radar is facing.
getRadarAbsHeading :: Robo s Angle
getRadarAbsHeading = do
  botHeading <- getHeading
  radarHeading <- getRadarRelHeading
  return $ botHeading + radarHeading

-- | Utility function to convert an angle accessor to one that returns a vector
-- in the direction of the angle instead.
angleAsVec :: Robo s Angle -> Robo s Vec
angleAsVec = (vecFromAngle <$>)

---------------------------------
--  CONTROL
---------------------------------

-- | Set the engine thrust, which controls the robot's forward (or backward if negative)
-- acceleration.  Limited by game rules.
setThrust :: Scalar -> Robo s ()
setThrust = setBotCapped ruleMaxThrust botThrust

-- | Set the turning power in the anticlockwise direction (or clockwise if negative)
-- Limited by game rules.
setTurnPower :: Scalar -> Robo s ()
setTurnPower = setBotCapped ruleMaxAngThrust botAngThrust

-- | Set the rotation speed of the gun (positive is anticlockwise).
setGunSpeed :: Scalar -> Robo s ()
setGunSpeed = setBotCapped ruleMaxGunSpeed (botGun.gunAngVel)

-- | Set the firing power of the gun, where 0 means don't fire. Limited by game rules.
setFiring :: Scalar -> Robo s ()
setFiring power = withBot $ do
  min <- asks (view ruleMinFirePower)
  max <- asks (view ruleMaxFirePower)
  case () of
    () | power < 0 -> botGun.gunFiring .= 0
       | power > 0 && power < min -> botGun.gunFiring .= min
       | power > max -> botGun.gunFiring .= max
       | otherwise -> botGun.gunFiring .= power

-- | Set the rotation speed of the radar (positive is anticlockwise).
setRadarSpeed :: Scalar -> Robo s ()
setRadarSpeed = setBotCapped ruleMaxRadSpeed (botRadar.radAngVel)

---------------------------------
--  LOGGING
---------------------------------

-- | Log a string.
printLine :: String -> Robo s ()
printLine message = withBot (tell [message])

-- | Log something showable.
printShow :: Show a => a -> Robo s ()
printShow = printLine . show

---------------------------------
--  UTILITY FUNCTIONS
---------------------------------

instance MonadRandom BotWrapper where
  getRandom   = BotWrapper getRandom
  getRandoms  = BotWrapper getRandoms
  getRandomR  = BotWrapper . getRandomR
  getRandomRs = BotWrapper . getRandomRs

withBot :: Bot a -> Robo s a
withBot bot = lift (BotWrapper bot)

useBot :: FoldLike a BotState a' a b' -> Robo s a
useBot = withBot . use

setBotCapped :: (Num a, Ord a) => Getter' BattleRules a -> Setter' BotState a -> a -> Robo s ()
setBotCapped capBy setter val = withBot (setCapped capBy setter val)

setCapped :: (Num a, Ord a) => Getter' BattleRules a -> Setter' BotState a -> a -> Bot ()
setCapped capBy setter val = do
  limit <- asks (view capBy)
  case () of
    () | val > limit  -> setter .= limit
       | val < -limit -> setter .= -limit
       | otherwise    -> setter .= val
