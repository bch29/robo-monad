{-# LANGUAGE TemplateHaskell
           , ExistentialQuantification
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , FlexibleInstances #-}

module Game.Robo.Core.Types
  -- ( Bot
  -- , Robo (..)
  -- , BotSpec (..)
  -- , BotState, botThrust, botAngThrust, botPos, botVel, botMass
  -- , Rules, ruleMaxThrust, ruleMaxAngThrust, ruleMass
  -- , Vec , vec
  -- , Angle
  -- , Rect, rect, rectCentre, rectSize, rectAngle )
    where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Random

import Lens.Family2
import Lens.Family2.TH

import Data.Vector.V2
import Data.Vector.Class

import Control.DeepSeq

---------------------------------
--  Rules
---------------------------------

-- | Specifies the parameters that robots operate under.
data Rules = Rules
  { _ruleMaxThrust          :: Scalar -- The maximum engine power allowed.
  , _ruleMaxAngThrust       :: Scalar -- The maximum turning power allowed.
  , _ruleMaxGunTurnPower    :: Scalar -- The maximum angular acceleration of the gun.
  , _ruleMaxRadSpeed        :: Scalar -- The maximum rotation speed of the radar.
  , _ruleMaxFirePower       :: Scalar -- The maximum firing power.
  , _ruleMinFirePower       :: Scalar -- The minimum firing power.
  , _ruleMass               :: Scalar -- The mass of each robot.
  , _ruleDriveFriction      :: Scalar -- The friction experienced by the robots in moving forward.
  , _ruleTurnFriction       :: Scalar -- The friction experienced by the robots in turning.
  , _ruleGunFriction        :: Scalar -- The friction experienced by the robots' guns in turning.
  , _ruleMaxEnergy          :: Scalar -- The maximum energy that a robot can have.
  , _ruleEnergyRechargeRate :: Scalar -- The rate at which energy recharges, per second.
  , _ruleBotSize            :: Vec    -- The vector dimensions of the robots.
  , _ruleGunSize            :: Vec    -- The vector dimensions of the guns.
  , _ruleRadarSize          :: Vec    -- The vector dimensions of the radars.
  , _ruleBulletSpeed        :: Scalar -- The speed at which bullets move.
  , _ruleRadRange           :: Scalar -- The range at which radars are able to see robots.
  , _ruleRadFOV             :: Angle  -- The angle through which radars are able to see robots.
  , _ruleArenaSize          :: Vec    -- The vector dimensions of the arena.
  , _ruleSpawnMargin        :: Scalar -- The smallest distance from the edge of arena that robots can spawn at.
  , _ruleStepInterval       :: Int    -- The time between updates of the simulation, in milliseconds.
  , _ruleTickSteps          :: Int    -- The number of steps between each Robo tick.
  }

---------------------------------
--  Monads
---------------------------------

-- | The monad exposed to the user, which allows a restricted set of
-- functions to modify the underlying monad. Parametrised by user state.
newtype Robo s a = Robo (StateT s BotWrapper a)
  deriving (Monad, Functor, Applicative, MonadRandom)

-- | Wraps the Bot monad so that we can have a different StateT for user state.
newtype BotWrapper a = BotWrapper { runBotWrapper :: Bot a }
  deriving (Monad, Functor, Applicative, MonadRandom)

-- | The internal robot monad.
type Bot = PureContext BotState

-- | A bot that can do I/O.
type IOBot = ContextT BotState IO

-- | The internal world monad.
type World = PureContext WorldState

-- | A world that can do I/O.
type IOWorld = ContextT WorldState IO

-- | A pure context, based on Rand.
type PureContext s = ContextT s (Rand StdGen)

-- | A lot of computations take place within this context, with a Writer
-- for logging, a Reader to keep track of the battle rules and a Rand for
-- random number generation.
type ContextT s m = StateT s (WriterT [String] (ReaderT Rules m))

---------------------------------
--  Robot State
---------------------------------

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

-- | Specifies a robot's behaviour by providing controller functions to react to events
-- and a few bits of information about the robot.
data BotSpec = forall s. BotSpec
  { botName :: String               -- The robot's name.
  , botInitialState :: s            -- The initial state.
  , onInit :: Robo s ()             -- When the robot is initialised.
  , onTick :: Robo s ()             -- When a game tick passes.
  , onScan :: ScanData -> Robo s () -- When the radar scans another bot.
  , onHitByBullet :: Robo s ()      -- When the robot is hit by an enemy bullet.
  , onBulletHit   :: Robo s ()      -- When a bullet fired by the robot hits a target.
  }

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
  { scanDistance :: !Scalar -- The distance to the scanned robot from the scanning robot.
  , scanAngle    :: !Angle  -- The angle of the scanned robot relative to the scanning robot.
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
--  Core Types
---------------------------------

-- | The type used for robot IDs.
type BotID = Int

-- | A two-dimensional real-valued vector.
type Vec = Vector2

-- | Make a vector from two scalars @x@ and @y@.
vec :: Scalar -> Scalar -> Vec
vec = Vector2

-- | An angle in radians.
type Angle = Scalar

-- | A rectangle with a centre, size and angle (to the horizontal).
data Rect = Rect
  { _rectCentre :: !Vec
  , _rectSize   :: !Vec
  , _rectAngle  :: !Angle
  }

-- | Make a rectangle from a centre, size and angle.
rect :: Vec -> Vec -> Angle -> Rect
rect = Rect

-- | Make a rectangle from two corners, with no rotation.
rectFromCorners :: Vec -> Vec -> Rect
rectFromCorners a b = rect centre size 0
  where size = abs $ b - a
        centre = (a + b) |* 0.5

---------------------------------
--  Instances
---------------------------------

instance MonadState s (Robo s) where
  get = Robo get
  put s = Robo (put s)

instance Show Rect where
  showsPrec prec (Rect cn sz ang) = showParen (prec /= 0) res
    where res = ("Rect " ++) . svec cn . spc . svec sz . spc . showsPrec (prec + 2) ang
          svec (Vector2 x y) = showParen True $
            showString "vec " .
            showsPrec (prec + 1) x .
            spc .
            showsPrec (prec + 1) y
          spc = (' ':)

instance Random Vector2 where
  random gen = (vec x y, gen'')
    where (x, gen')  = random gen
          (y, gen'') = random gen'

  randomR (Vector2 x1 y1, Vector2 x2 y2) gen = (vec x y, gen'')
    where (x, gen')  = randomR (x1, x2) gen
          (y, gen'') = randomR (y1, y2) gen'

instance NFData Vector2 where
  rnf (Vector2 x y) = x `seq` y `seq` ()

instance NFData Bullet where
  rnf bul = rnf (_bulVel bul)
      `seq` rnf (_bulPos bul)

instance NFData BotState where
  rnf bot = rnf (_botPos bot)

instance NFData WorldState where
  rnf wld = wld `seq` ()

---------------------------------
--  Lenses
---------------------------------

makeLenses ''WorldState

makeLenses ''BotState
makeLenses ''GunState
makeLenses ''RadarState

makeLenses ''Bullet
makeLenses ''BulletCollision

makeLenses ''Rect

makeLensesFor [("v2x", "vX"), ("v2y", "vY")] ''Vector2
