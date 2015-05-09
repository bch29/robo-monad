module Game.Robo.Core
  ( defaultRules
  , runContext
  , evalContext
  , applyBot
  , runRobo
  , runIOContext
  , evalIOContext
  , promoteContext
  , module Game.Robo.Core.Types
  , UpdateChan
  , ResponseChan
  )
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
import Game.Robo.Draw.DrawWorld
import Game.Robo.Maths

type UpdateChan = Chan (BotState, WorldState, Double, Bool)
type ResponseChan = Chan (BotID, BotState, [Bullet])

defaultRules :: BattleRules
defaultRules =
  BattleRules { _ruleMaxThrust     = 500
              , _ruleMaxAngThrust  = 32
              , _ruleMaxGunSpeed   = 4
              , _ruleMaxRadSpeed   = 16
              , _ruleMaxFirePower  = 2
              , _ruleMinFirePower  = 0.5
              , _ruleMass          = 1
              , _ruleDriveFriction = 0.98
              , _ruleTurnFriction  = 0.9
              , _ruleBotSize       = vec 60 40
              , _ruleGunSize       = vec 40 8
              , _ruleRadarSize     = vec 10 30
              , _ruleBulletSpeed   = 400
              , _ruleRadRange      = 2000
              , _ruleRadFOV        = pi / 6
              , _ruleArenaSize     = vec 800 800
              , _ruleSpawnMargin   = 100
              , _ruleTickTime      = 0.1
              }

-- | Evaluate a Bot monadic action in the context of its world.
applyBot :: BotID -> Bot a -> World a
applyBot bid bot = do
  botStates <- use wldBots
  let (prevStates, targetState : postStates) = splitAt (bid - 1) botStates
  (result, newState) <- lift $ runStateT bot targetState
  wldBots .= prevStates ++ newState : postStates
  return result

-- | Evaluate a Robo down to its underlying Bot monad.
runRobo :: Robo s a -> s -> Bot (a, s)
runRobo ctrl state = runBotWrapper (runStateT ctrl state)

-- | Runs an action in a stateful context, returning the resulting state,
-- random number generator and message log along with the result of the action.
runContext :: StatefulContext s a -> StdGen -> BattleRules -> s -> (a, s, [String], StdGen)
runContext ctx gen rules state = (res, state', log, gen')
  where noState  = runStateT  ctx state
        noWriter = runWriterT noState
        noReader = runReaderT noWriter rules
        (((res, state'), log), gen') = runRand noReader gen

-- | Evaluates an action in a stateful context, returning the resulting state and
-- message log along with the action's result.
evalContext :: StatefulContext s a -> StdGen -> BattleRules -> s -> (a, s, [String])
evalContext ctx gen rules state =
  let (res, state', log, _) = runContext ctx gen rules state
  in  (res, state', log)

-- | Runs an IOWorld action, returning the results, state and message log.
runIOContext :: IOContext s a -> BattleRules -> s -> IO (a, s, [String])
runIOContext ctx rules state = do
  let noState = runStateT ctx state
      noWriter = runWriterT noState
      noReader = runReaderT noWriter rules
  ((res, state'), log) <- noReader
  return (res, state', log)

-- | Evaluates an IOContext action and returns just the result.
evalIOContext :: IOContext s a -> BattleRules -> s -> IO a
evalIOContext ctx rules state = do
  (res, _, _) <- runIOContext ctx rules state
  return res

-- | Promotes a stateful context to one able to do I/O.
promoteContext :: StatefulContext s a -> IOContext s a
promoteContext ctx = do
  state <- get
  rules <- ask
  gen   <- liftIO getStdGen
  let (res, state', log, gen') = runContext ctx gen rules state
  liftIO $ setStdGen gen'
  put state'
  tell log
  return res
