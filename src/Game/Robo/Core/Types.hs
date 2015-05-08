{-# LANGUAGE TemplateHaskell, ExistentialQuantification, GeneralizedNewtypeDeriving #-}

module Game.Robo.Core.Types
  -- ( Bot
  -- , Robo (..)
  -- , BotSpec (..)
  -- , BotState, botThrust, botAngThrust, botPos, botVel, botMass
  -- , BattleRules, ruleMaxThrust, ruleMaxAngThrust, ruleMass
  -- , Vec , vec
  -- , Angle
  -- , Rect, rect, rectCentre, rectSize, rectAngle )
    where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Random
import System.Random

import Lens.Family2
import Lens.Family2.TH

import Data.Vector.V2
import Data.Vector.Class

---------------------------------
--  Monads
---------------------------------

-- | The monad exposed to the user, which allows a restricted set of
-- functions to modify the underlying State monad. Parametrised by user
-- state.
type Robo s = StateT s BotWrapper

-- | The internal robot monad. Uses a Writer for logging, a Reader to see
-- the game rules and a State for the current robot state.
type Bot = GameMonad BotState

-- | Wraps the Bot monad to prevent user access to internals.
newtype BotWrapper a = BotWrapper { runBotWrapper :: Bot a }
  deriving (Monad, Functor, Applicative)

-- | The internal world monad.
type World = GameMonad WorldState

-- | There is a lot shared between World and Bot, so why not factor it out?
type GameMonad s = RandT StdGen (WriterT [String] (ReaderT BattleRules (State s)))

---------------------------------
--  Robot State
---------------------------------

type BotID = Int

-- | State information for a robot.
data BotState =
     BotState { _botID        :: BotID
              , _botThrust    :: Scalar
              , _botAngThrust :: Scalar
              , _botPos       :: Vec
              , _botHeading   :: Angle
              , _botSpeed     :: Scalar
              , _botAngVel    :: Scalar
              , _botGun       :: GunState
              , _botMass      :: Scalar }

-- | Specifies a robot's behaviour by providing controller functions to react to events
-- and a few bits of information about the robot.
data BotSpec = forall s.
     BotSpec { botName :: String                    -- The robot's name.
             , botInitialState :: s                 -- The initial state.
             , onInit :: Robo s ()             -- When the robot is initialised.
             , onTick :: Robo s ()             -- When a game tick passes.
             , onScan :: ScanData -> Robo s () -- When the radar scans another bot.
             }

---------------------------------
--  Robot Substate
---------------------------------

-- | A robot's gun state.
data GunState =
     GunState { _gunHeading :: Angle
              , _gunAngVel  :: Scalar
              , _gunFiring  :: Scalar }

-- | Data received from scanning an enemy robot.
data ScanData = ScanData Int

---------------------------------
--  Bullets
---------------------------------

-- | A bullet.
data Bullet =
     Bullet { _bulVel   :: Vec
            , _bulPos   :: Vec
            , _bulPower :: Scalar
            , _bulOwner :: BotID }

---------------------------------
--  World State
---------------------------------

-- | Specifies the rules of the battle.
data BattleRules =
     BattleRules { _ruleMaxThrust     :: Scalar
                 , _ruleMaxAngThrust  :: Scalar
                 , _ruleMaxGunSpeed   :: Scalar
                 , _ruleMaxFirePower  :: Scalar
                 , _ruleMinFirePower  :: Scalar
                 , _ruleMass          :: Scalar
                 , _ruleDriveFriction :: Scalar
                 , _ruleTurnFriction  :: Scalar
                 , _ruleBotSize       :: Vec
                 , _ruleGunSize       :: Vec
                 , _ruleBulletSpeed   :: Scalar
                 , _ruleWorldSize     :: Vec
                 , _ruleTickTime      :: Double
                 }

-- | A convenient monad used for drawing stuff.
type DrawWorld = StateT WorldState (ReaderT BattleRules IO) ()

-- | State information for the world in which the battle is taking place.
data WorldState =
     WorldState { _wldBullets :: [Bullet]
                , _wldBots    :: [BotState]
                , _wldRect    :: Rect }

---------------------------------
--  Core Types
---------------------------------

-- | A two-dimensional real-valued vector.
type Vec = Vector2

-- | Make a vector from two scalars @x@ and @y@.
vec :: Scalar -> Scalar -> Vec
vec = Vector2

-- | An angle in radians.
type Angle = Scalar

-- | A rectangle with a centre, size and angle (to the horizontal).
data Rect = Rect { _rectCentre :: Vec
                 , _rectSize   :: Vec
                 , _rectAngle  :: Angle }

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

instance Show Rect where
  showsPrec prec (Rect cn sz ang) = showParen (prec /= 0) res
    where res = ("Rect " ++) . svec cn . spc . svec sz . spc . showsPrec (prec + 2) ang
          svec (Vector2 x y) = showParen True $
            showString "vec " .
            showsPrec (prec + 1) x .
            spc .
            showsPrec (prec + 1) y
          spc = (' ':)

---------------------------------
--  Lenses
---------------------------------

makeLenses ''BotState
makeLenses ''GunState
makeLenses ''Bullet
makeLenses ''BattleRules
makeLenses ''WorldState

makeLenses ''Rect

makeLensesFor [("v2x", "vX"), ("v2y", "vY")] ''Vector2
