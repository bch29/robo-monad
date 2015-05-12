{-|
Module      : ExampleBot
Description : A robot that does nothing and can be used as a template for your own robots.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable (depends on SDL)

-}

{-# LANGUAGE TemplateHaskell #-}

module ExampleBot (examplebot) where

-- Import of Game.Robo required, Maths and PidController
-- provide handy utility functions.
import Game.Robo
import Game.Robo.Maths
import Game.Robo.PidController

-- | Provides a handy alias for this robot's Robo monad.
type ExampleBot = Robo ExampleBotState

-- | Put all of the variables that you need here, prefixed by
-- underscores (so that template haskell can convert them to
-- lenses).
data ExampleBotState = ExampleBotState
  { _someState :: Int
  }

-- | Give fields their initial values.
emptyState :: ExampleBotState
emptyState = ExampleBotState
  { _someState = 0
  }

-- This converts fields in ExampleBotState to lenses which can be used
-- to easily access variables.
makeLenses ''ExampleBotState

-- | Runs when the bot is first created.
myInit :: ExampleBot ()
myInit = do
  return ()

-- | Runs every game tick.
myTick :: ExampleBot ()
myTick = do
  return ()

-- | Runs when the radar passes over an enemy robot.
myScan :: ScanData -> ExampleBot ()
myScan s = do
  return ()

-- | Runs when the robot is hit by an enemy bullet.
myOnHitByBullet :: ExampleBot ()
myOnHitByBullet = do
  return ()

-- | Runs when a bullet fired by this robot hits an enemy.
myOnBulletHit :: ExampleBot ()
myOnBulletHit = do
  return ()

-- | This is the actual robot specification to be passed to
-- @runWorld@.
examplebot :: BotSpec
examplebot = BotSpec
  { botName         = "examplebot"
  , botInitialState = emptyState
  , onInit          = myInit
  , onTick          = myTick
  , onScan          = myScan
  , onHitByBullet   = myOnHitByBullet
  , onBulletHit     = myOnBulletHit
  }
