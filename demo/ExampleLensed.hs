{-|
Module      : ExampleBot
Description : A robot that does nothing and can be used as a template for your own robots. Uses lenses.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable (depends on SDL)

-}

{-# LANGUAGE TemplateHaskell #-}
module ExampleLensed (examplebot) where

-- Import of Game.Robo required, Maths and PidController
-- provide handy utility functions.
import Game.Robo
import Game.Robo.Maths
import Game.Robo.PIDLensed

-- | Provides a handy alias for this robot's Robo monad.
type ExampleLensed = Robo ExampleLensedState

-- | Put all of the variables that you need here, prefixed by
-- underscores (so that template haskell can convert them to
-- lenses).
data ExampleLensedState = ExampleLensedState
  { _someState :: Int
  }

-- | Give fields their initial values.
emptyState :: ExampleLensedState
emptyState = ExampleLensedState
  { _someState = 0
  }

-- This converts fields in ExampleLensedState to lenses which can be used
-- to easily access variables.
makeLenses ''ExampleLensedState

-- | Runs when the bot is first created.
myInit :: ExampleLensed ()
myInit = do
  return ()

-- | Runs every game tick.
myTick :: ExampleLensed ()
myTick = do
  return ()

-- | Runs when the radar passes over an enemy robot.
myScan :: ScanData -> ExampleLensed ()
myScan s = do
  return ()

-- | Runs when the robot is hit by an enemy bullet.
myOnHitByBullet :: ExampleLensed ()
myOnHitByBullet = do
  return ()

-- | Runs when a bullet fired by this robot hits an enemy.
myOnBulletHit :: ExampleLensed ()
myOnBulletHit = do
  return ()

-- | Runs when this robot collides with the arena walls.
myOnCollideWall :: WallCollisionData -> ExampleLensed ()
myOnCollideWall w = do
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
  , onCollideWall   = myOnCollideWall
  }
