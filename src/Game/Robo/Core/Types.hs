{-|
Module      : Game.Robo.Core.Types
Description : Most of the types required throughout.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

These are defined in their own file to avoid module import cycles.
-}

{-# LANGUAGE ExistentialQuantification  #-} -- Note [ExistentialQuantification]
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- For easy newtype wrapping
{-# LANGUAGE MultiParamTypeClasses      #-} -- For MonadState instance of @Robo s@
{-# LANGUAGE FlexibleInstances          #-} -- For MonadState instance of @Robo s@

module Game.Robo.Core.Types
  ( Rules (..)

  , Robo  (..), BotWrapper (..)
  , Bot, IOBot, DrawBot
  , World, IOWorld, DrawWorld
  , PureContext, DrawContext, ContextT

  , WorldState (..)

  , BotID
  , BotState (..), BotSpec (..)

  , GunState (..), RadarState      (..)
  , Bullet   (..), BulletCollision (..)
  , ScanData (..), WallCollisionData (..)

  , BotUpdate (..), BotResponse (..)

  , module Game.Robo.Core.Types.Maths
  ) where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Random

import Lens.Family2

import Control.DeepSeq

import Game.Robo.Render
import Game.Robo.Core.Types.Maths

---------------------------------
--  Rules
---------------------------------

-- | Specifies the parameters of the simulation.
data Rules = Rules
  {
  -- | The vector dimensions of the robots.
    _ruleBotSize            :: Vec
  -- | The maximum engine power allowed.
  , _ruleMaxThrust          :: Scalar
  -- | The friction experienced by the robots in moving forward.
  , _ruleDriveFriction      :: Scalar
  -- | The maximum turning power allowed.
  , _ruleMaxAngThrust       :: Scalar
  -- | The friction experienced by the robots in turning.
  , _ruleTurnFriction       :: Scalar
  -- | The mass of each robot.
  , _ruleMass               :: Scalar
  -- | The maximum energy that a robot can have.
  , _ruleMaxEnergy          :: Scalar
  -- | The rate at which energy recharges, per second.
  , _ruleEnergyRechargeRate :: Scalar

  -- | The vector dimensions of the radars.
  , _ruleRadarSize          :: Vec
  -- | The maximum rotation speed of the radar.
  , _ruleMaxRadSpeed        :: Scalar
  -- | The range at which radars are able to see robots.
  , _ruleRadRange           :: Scalar
  -- | The angle through which radars are able to see robots.
  , _ruleRadFOV             :: Angle

  -- | The vector dimensions of the guns.
  , _ruleGunSize            :: Vec
  -- | The maximum angular acceleration of the gun.
  , _ruleMaxGunTurnPower    :: Scalar
  -- | The friction experienced by the robots' guns in turning.
  , _ruleGunFriction        :: Scalar
  -- | The speed at which bullets move.
  , _ruleBulletSpeed        :: Scalar
  -- | The maximum firing power.
  , _ruleMaxFirePower       :: Scalar
  -- | The minimum firing power.
  , _ruleMinFirePower       :: Scalar

  -- | The vector dimensions of the arena.
  , _ruleArenaSize          :: Vec
  -- | The smallest distance from the edge of arena that robots can spawn at.
  , _ruleSpawnMargin        :: Scalar
  -- | The time between updates of the simulation, in milliseconds.
  , _ruleStepInterval       :: Int
  -- | The number of steps between each Robo tick.
  , _ruleTickSteps          :: Int
  }

---------------------------------
--  Monads
---------------------------------

-- | The monad in which robots run. Parametrised by user state.
newtype Robo s a = Robo (StateT s BotWrapper a)
  deriving (Monad, Functor, Applicative, MonadRandom)

-- | Wraps the Bot monad so that we can have a different StateT for user state.
newtype BotWrapper a = BotWrapper { runBotWrapper :: Bot a }
  deriving (Monad, Functor, Applicative, MonadRandom)

-- | The internal robot monad.
type Bot = PureContext BotState

-- | A bot that can do I/O.
type IOBot = ContextT BotState IO

-- | A bot that can draw.
type DrawBot = DrawContext BotState

-- | The internal world monad.
type World = PureContext WorldState

-- | A world that can do I/O.
type IOWorld = ContextT WorldState IO

-- | A world that can draw.
type DrawWorld = DrawContext WorldState

-- | A pure context, based on Rand.
type PureContext s = ContextT s (Rand StdGen)

-- | A context that can draw.
type DrawContext s = ContextT s (RandT StdGen Draw)

-- | A lot of computations take place within this context, with a Writer
-- for logging, a Reader to keep track of the battle rules and a Rand for
-- random number generation. We don't use RWS because we want StateT as
-- the outer layer so that we can easily strip off a BotState and replace
-- it with a WorldState to 'promote' Bot to World.
type ContextT s m = StateT s (WriterT [String] (ReaderT Rules m))

---------------------------------
--  World State
---------------------------------

-- | State information for the world in which the battle is taking place.
data WorldState = WorldState
  { _wldBullets   :: ![Bullet]   -- The bullets fired by robots.
  , _wldBots      :: ![BotState] -- The robots themselves.
  , _wldRect      :: !Rect       -- Represents the size of the arena.
  , _wldTime      :: !Int        -- The time in milliseconds since simulation started.
  , _wldSinceStep :: !Int        -- The number of milliseconds since the last step.
  , _wldSinceTick :: !Int        -- The number of steps that have passed since the last tick.
  }

---------------------------------
--  Robot State
---------------------------------

-- | The type used for robot IDs.
type BotID = Int

-- | State information for a robot.
data BotState = BotState
  { _botID        :: !BotID      -- The ID number of the robot.
  , _botThrust    :: !Scalar     -- The engine power.
  , _botAngThrust :: !Scalar     -- The turning power.
  , _botPos       :: !Vec        -- The position in pixels relative to the top-left corner.
  , _botHeading   :: !Angle      -- The angle in radians that the robot is facing.
  , _botSpeed     :: !Scalar     -- The current speed.
  , _botAngVel    :: !Scalar     -- The current angular velocity.
  , _botGun       :: !GunState   -- The state of the robot's gun.
  , _botRadar     :: !RadarState -- The state of the robot's radar.
  , _botMass      :: !Scalar     -- The mass of the robot.
  , _botEnergy    :: !Scalar     -- The current energy the robot has available.
  }

-- | Specifies a robot's behaviour.
data BotSpec = forall s. BotSpec -- Note [ExistentialQuantification]
  {
  -- | The robot's name.
    botName :: String
  -- | The initial state.
  , botInitialState :: s
  -- | Executed when the robot is initialised.
  , onInit :: Robo s ()
  -- | Executed when a game tick passes.
  , onTick :: Robo s ()
  -- | Executed when the radar scans another bot.
  , onScan :: ScanData -> Robo s ()
  -- | Executed when this robot is hit by an enemy bullet.
  , onHitByBullet :: Robo s ()
  -- | Executed when a bullet fired by this robot hits a target.
  , onBulletHit   :: Robo s ()
  -- | Executed when the robot collides with a wall.
  , onCollideWall :: WallCollisionData -> Robo s ()
  }

{-
Note [ExistentialQuantification]

We do forall s. BotSpec in order to allow each robot to have its own
distinct state structure. This is possible without breaking the type
system because only the BotSpec knows about @s@. The user's code can
do whatever it wants with @s@, and all our code (in Game.Robo.Core.Bot)
needs to know is that the @s@ is consistent.
-}

---------------------------------
--  Robot Substate
---------------------------------

-- | A robot's gun state.
data GunState = GunState
  { _gunHeading :: !Angle  -- The direction relative to the robot that the gun is facing.
  , _gunAngAcc  :: !Scalar -- The rate at which the gun's rate of turning is increasing.
  , _gunAngVel  :: !Scalar -- The speed at which the gun is turning.
  , _gunFiring  :: !Scalar -- If 0, not firing. Otherwise, the power of the next shot.
  }

-- | A robot's radar state.
data RadarState = RadarState
  { _radHeading :: !Angle  -- The direction relative to the robot that the radar is facing.
  , _radAngVel  :: !Scalar -- The speed at which the radar is turning.
  }

-- | Data received from scanning an enemy robot.
data ScanData = ScanData
  {
  -- | The distance to the scanned robot from the scanning robot.
    scanDistance :: !Scalar
  -- | The absolute angle of the scanned robot from the scanning robot.
  , scanAngle    :: !Angle
  }

-- | Data received when colliding with a wall.
data WallCollisionData = WallCollisionData
  {
  -- | The angle that the robot makes with the hit wall.
    wcolAngle :: !Angle
  }

---------------------------------
--  Bullets
---------------------------------

-- | A bullet.
data Bullet = Bullet
  { _bulVel   :: !Vec    -- The bullet's velocity.
  , _bulPos   :: !Vec    -- The bullet's position in the world.
  , _bulPower :: !Scalar -- The bullet's fire power.
  , _bulOwner :: !BotID  -- The ID of the robot that fired the bullet.
  }

-- | Information about a bullet collision.
data BulletCollision = BulletCollision
  { _bcolAggressor :: !BotID  -- The ID of the robot that fired the bullet.
  , _bcolVictim    :: !BotID  -- The ID of the robot that was hit by the bullet.
  , _bcolPower     :: !Scalar -- The power that the bullet was fired with.
  }

---------------------------------
--  Thread Communication
---------------------------------

data BotUpdate = BotUpdate
  { updateState            :: !BotState          -- The robot state at the start of the update.
  , updateWorld            :: !WorldState        -- The world state at the start of the update.
  , updatePassed           :: !Double            -- The amount of time since the last update.
  , updateDoTick           :: !Bool              -- Should the robot run its onTick?
  , updateBulletCollisions :: ![BulletCollision] -- Was the robot hit by a bullet?
  }

data BotResponse = BotResponse
  { responseID      :: !BotID    -- The ID of the robot responding.
  , responseState   :: !BotState -- The new state of the robot after an update.
  , responseBullets :: ![Bullet] -- The bullets fired by the robot during the update.
  }

---------------------------------
--  Instances
---------------------------------

instance MonadState s (Robo s) where
  get = Robo get
  put s = Robo (put s)

instance NFData Bullet where
  rnf bul = rnf (_bulVel bul)
      `seq` rnf (_bulPos bul)

instance NFData BotState where
  rnf bot = rnf (_botPos bot)

instance NFData WorldState where
  rnf wld = wld `seq` ()
