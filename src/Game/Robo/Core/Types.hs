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

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Game.Robo.Core.Types
  ( Rules (..)

  , Robo (..), RoboF (..)
  , Bot, IOBot
  , World, IOWorld
  , PureRoboContext

  , RoboContextT

  , GameActions (..)

  , WorldState (..)

  , BotID
  , BotState (..), BotSpec (..)

  , GunState (..), RadarState      (..)
  , Bullet   (..), BulletCollision (..)
  , ScanData (..), WallCollisionData (..)

  , BotUpdate (..), BotResponse (..)

  , StB, StW, Ru, Ra, Wr, MIO

  , module Many
  , module Game.Robo.Core.Types.Maths
  ) where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad.Free.Church  (F (..), liftF)
import           Control.Monad.Random
import           Control.Monad.RWS.Strict
import           GHC.Generics               (Generic)

import           Game.Robo.Core.Many.Vector as Many
import           Game.Robo.Core.Types.Maths

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

  -- | The size of the life bar (when at full life)
  , _ruleLifebarSize        :: Vec
  -- | The position of the life bar relative to the centre of the associated robot.
  , _ruleLifebarOffset      :: Vec
  -- | The maximum (starting) life of robots.
  , _ruleMaxLife            :: Scalar

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
  -- | The minimum number of steps per second.
  , _ruleMinSPS             :: Int
  -- | The maximum number of steps per second.
  , _ruleMaxSPS             :: Int
  -- | The default (starting) number of steps per second.
  , _ruleDefaultSPS         :: Int
  -- | The number of steps between each Robo tick.
  , _ruleTickSteps          :: Int
  }

---------------------------------
--  Monads
---------------------------------

-- | The main Robo Monad. Parameterised by user state. Newtyped for better type
-- errors.
newtype Robo s a = Robo (F (RoboF s) a)

deriving instance Functor (Robo s)
deriving instance Applicative (Robo s)
deriving instance Monad (Robo s)

-- | The functor used for the free monad representation of robot actions.
data RoboF s a where
  GetUStateR :: (s -> a)                         -> RoboF s a
  PutUStateR :: s -> a                           -> RoboF s a
  GetIStateR :: (BotState -> a)                  -> RoboF s a
  PutIStateR :: BotState -> a                    -> RoboF s a
  RulesR     :: (Rules -> a)                     -> RoboF s a
  LogR       :: String -> a                      -> RoboF s a
  RandVR     :: Random r =>           (r   -> a) -> RoboF s a
  RandsVR    :: Random r =>           ([r] -> a) -> RoboF s a
  RandIR     :: Random r => (r, r) -> (r   -> a) -> RoboF s a
  RandsIR    :: Random r => (r, r) -> ([r] -> a) -> RoboF s a

deriving instance Functor (RoboF s)

-- | The internal robot monad.
type Bot = PureRoboContext BotState

-- | A bot that can do I/O.
type IOBot = RoboContextT BotState IO

-- | The internal world monad.
type World = PureRoboContext WorldState

-- | A world that can do I/O.
type IOWorld = RoboContextT WorldState IO

-- | A pure context, based on Rand.
type PureRoboContext s = RoboContextT s (Rand StdGen)

-- | A lot of computations take place within this context, with a Writer
-- for logging, a Reader to keep track of the battle rules and a Rand for
-- random number generation. We don't use RWS because we want StateT as
-- the outer layer so that we can easily strip off a BotState and replace
-- it with a WorldState to 'promote' Bot to World.
type RoboContextT s = RWST Rules String s

-- Here we define some aliases for typeclass constraints that are used a lot.
type StB = MonadState BotState
type StW = MonadState WorldState
type Ru  = MonadReader Rules
type Ra  = MonadRandom
type Wr  = MonadWriter String
type MIO = MonadIO

---------------------------------
--  Main Game Engine
---------------------------------

data GameActions s = GameActions
  {
    -- | The action which is called as the game is initialising.
    actionInit     :: Maybe (RoboContextT s IO ())
    -- | The main action which is continually called while doing nothing else.
  , actionMain     :: Maybe (RoboContextT s IO ())
    -- | The action that is called when a key is pressed.
  , actionKeyboard :: Maybe (Char -> RoboContextT s IO ())
  }

---------------------------------
--  World State
---------------------------------

-- | State information for the world in which the battle is taking place.
data WorldState = WorldState
  { _wldBullets      :: !(Many Bullet)   -- The bullets fired by robots.
  , _wldBots         :: !(Many BotState) -- The robots themselves.
  , _wldRect         :: !Rect       -- Represents the size of the arena.
  , _wldTime0        :: !Int        -- The time when the SPS was last changed.
  , _wldTime         :: !Int        -- The time in milliseconds since simulation started.
  , _wldStepsDone    :: !Int        -- The number of steps done since the SPS was last changed.
  , _wldSPS          :: !Int        -- The current number of steps per second.
  , _wldSinceTick    :: !Int        -- The number of steps that have passed since the last tick.
  , _wldUpdateChans  :: !(Many (Chan BotUpdate)) -- The channels along which the world sends update requests to the bots.
     -- The channel along which the bots send update responses to the world.
  , _wldResponseChan :: !(Maybe (Chan BotResponse))
  }
  deriving (Generic)

---------------------------------
--  Robot State
---------------------------------

-- | The type used for robot IDs.
type BotID = Int

-- | State information for a robot.
data BotState = BotState
  { _botID        :: !BotID      -- The ID number of the robot.
  , _botTID       :: !(Maybe ThreadId) -- The ID of the thread in which the robot is running, if any.
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
  , _botLife      :: !Scalar     -- The amount of life left in the robot.
  }
  deriving (Generic)

-- | Specifies a robot's behaviour.
data BotSpec = forall s. BotSpec -- Note [ExistentialQuantification]
  {
  -- | The robot's name.
    botName         :: String
  -- | The initial state.
  , botInitialState :: s
  -- | Executed when the robot is initialised.
  , onInit          :: Robo s ()
  -- | Executed when a game tick passes.
  , onTick          :: Robo s ()
  -- | Executed when the radar scans another bot.
  , onScan          :: ScanData -> Robo s ()
  -- | Executed when this robot is hit by an enemy bullet.
  , onHitByBullet   :: Robo s ()
  -- | Executed when a bullet fired by this robot hits a target.
  , onBulletHit     :: Robo s ()
  -- | Executed when the robot collides with a wall.
  , onCollideWall   :: WallCollisionData -> Robo s ()
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
  deriving (Generic)

-- | A robot's radar state.
data RadarState = RadarState
  { _radHeading :: !Angle  -- The direction relative to the robot that the radar is facing.
  , _radAngVel  :: !Scalar -- The speed at which the radar is turning.
  }
  deriving (Generic)

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
  deriving (Generic, Eq, Ord)

-- | Information about a bullet collision.
data BulletCollision = BulletCollision
  { _bcolAggressor :: !BotID  -- The ID of the robot that fired the bullet.
  , _bcolVictim    :: !BotID  -- The ID of the robot that was hit by the bullet.
  , _bcolPower     :: !Scalar -- The power that the bullet was fired with.
  }
  deriving (Generic)

---------------------------------
--  Thread Communication
---------------------------------

data BotUpdate = BotUpdate
  { updateState            :: !BotState          -- The robot state at the start of the update.
  , updateWorld            :: !WorldState        -- The world state at the start of the update.
  , updatePassed           :: !Double            -- The amount of time since the last update.
  , updateDoTick           :: !Bool              -- Should the robot run its onTick?
  , updateBulletCollisions :: !(Many BulletCollision) -- Was the robot hit by a bullet?
  }
  deriving (Generic)

data BotResponse = BotResponse
  { responseID      :: !BotID    -- The ID of the robot responding.
  , responseState   :: !BotState -- The new state of the robot after an update.
  , responseBullets :: !(Many Bullet) -- The bullets fired by the robot during the update.
  }
  deriving (Generic)

---------------------------------
--  Instances
---------------------------------

instance NFData WorldState where
  rnf WorldState{..} =
    _wldBullets `deepseq`
    _wldBots `deepseq`
    ()

instance NFData Bullet
instance NFData RadarState
instance NFData GunState
instance NFData BotState
instance NFData BulletCollision
instance NFData BotUpdate
instance NFData BotResponse

-- We choose to instantiate 'MonadState' for the user state rather than the
-- internal state because the latter is not too sorely needed here and this way
-- we don't need to add a newtype wrapper over 'Robo' to allow the user to use
-- 'MonadState' functions, and the user can't access our internal state.
instance MonadState s (Robo s) where
  get = (Robo . liftF . GetUStateR) id
  put s = (Robo . liftF) (PutUStateR s ())

instance MonadRandom (Robo s) where
  getRandom         = (Robo . liftF . RandVR)        id
  getRandomR range  = (Robo . liftF . RandIR range)  id
  getRandoms        = (Robo . liftF . RandsVR)       id
  getRandomRs range = (Robo . liftF . RandsIR range) id
