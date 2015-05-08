module Game.Robo.Core
  ( defaultRules
  , evalBot
  , evalWorld
  , applyBot
  , runRobo
  , runDrawing
  , module Game.Robo.Core.Types )
    where

import Lens.Family2
import Lens.Family2.State

import Control.Concurrent

import Control.Applicative
import Data.Traversable
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Random

import Data.Vector.Class
import Data.List
import Data.Maybe

import Game.Robo.Core.Types
import Game.Robo.Core.DrawWorld
import Game.Robo.Maths

type UpdateChan = Chan (BotState, WorldState, Double, Bool)
type ResponseChan = Chan (BotID, BotState, [Bullet])

defaultRules :: BattleRules
defaultRules =
  BattleRules { _ruleMaxThrust     = 250
              , _ruleMaxAngThrust  = 32
              , _ruleMaxGunSpeed   = 2
              , _ruleMaxFirePower  = 2
              , _ruleMinFirePower  = 0.5
              , _ruleMass          = 1
              , _ruleDriveFriction = 0.98
              , _ruleTurnFriction  = 0.9
              , _ruleBotSize       = vec 60 40
              , _ruleGunSize       = vec 40 8
              , _ruleBulletSpeed   = 400
              , _ruleWorldSize     = vec 800 800
              , _ruleTickTime      = 0.1
              }

-- | Evaluates an action in the Bot monad, returning the resulting state and message log.
evalBot :: Bot a -> StdGen -> BattleRules -> BotState -> (a, BotState, [String])
evalBot = evalGameMonad

evalWorld :: World a -> StdGen -> BattleRules -> WorldState -> (a, WorldState, [String])
evalWorld = evalGameMonad

-- | Evaluate a Bot monadic action in the context of its world.
applyBot :: StdGen -> BotID -> Bot a -> World a
applyBot gen bid bot = do
  rules <- ask
  botStates <- use wldBots
  let (prevStates, targetState : postStates) = splitAt (bid - 1) botStates
      (result, newState, log) = evalBot bot gen rules targetState

  wldBots .= prevStates ++ newState : postStates
  tell log
  return result

-- | Evaluate a Robo down to its underlying Bot monad.
runRobo :: Robo s a -> s -> Bot (a, s)
runRobo ctrl state = runBotWrapper (runStateT ctrl state )

-- | Evaluate a DrawWorld monadic action.
runDrawing :: DrawWorld -> BattleRules -> WorldState -> IO ()
runDrawing drawing rules state = runReaderT (evalStateT drawing state) rules

-- | Evaluates an action in a game monad.
evalGameMonad :: GameMonad s a -> StdGen -> BattleRules -> s -> (a, s, [String])
evalGameMonad game gen rules state = (res, state', log)
  where noRandom = evalRandT game gen
        noWriter = runWriterT noRandom
        noReader = runReaderT noWriter rules
        ((res, log), state') = runState noReader state

