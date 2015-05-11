{-|
Module      : Game.Robo.Core
Description : Core operations shared by lots of parts of the game engine.
Copyright   : (c) Bradley Hardy, 2015
License     : BSD3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable (depends on SDL)

Mainly contains utility functions for unwrapping/lifting/promoting/handling monads,
re-exports Types, Lenses and Rules because almost everything that uses Core also
use all of those.
-}

module Game.Robo.Core
  ( applyBot
  , runRobo
  , runContext
  , evalContext
  , runPureContext
  , promoteContext
  , forceContext
  , whileContext
  , iterateContext
  , module Game.Robo.Core.Types
  , module Game.Robo.Core.Lenses
  , module Game.Robo.Core.Rules
  ) where

import Lens.Family2
import Lens.Family2.State

import Control.DeepSeq
import Control.Exception

import Control.Applicative
import Data.Traversable
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Random

import Data.List
import Data.Maybe

import Game.Robo.Core.Types
import Game.Robo.Core.Rules
import Game.Robo.Core.Lenses
import Game.Robo.Maths

-- | Evaluate a Bot monadic action in the context of its world.
applyBot :: Monad m => BotID -> ContextT BotState m a -> ContextT WorldState m a
applyBot bid bot = do
  botStates <- use wldBots
  let (prevStates, targetState : postStates) = splitAt (bid - 1) botStates
  (result, newState) <- lift $ runStateT bot targetState
  wldBots .= prevStates ++ newState : postStates
  return result

-- | Evaluate a Robo down to its underlying Bot monad.
runRobo :: Robo s a -> s -> Bot (a, s)
runRobo (Robo ctrl) state = runBotWrapper (runStateT ctrl state)

-- | Runs an action in a stateful context, returning the resulting state,
-- random number generator and message log along with the result of the action.
runPureContext :: PureContext s a -> StdGen -> Rules -> s -> (a, s, [String], StdGen)
runPureContext ctx gen rules state = (res, state', log, gen')
  where noCtx = runContext ctx rules state
        ((res, state', log), gen') = runRand noCtx gen

-- | Runs a contextual action, returning the results, state and message log.
runContext :: Monad m => ContextT s m a -> Rules -> s -> m (a, s, [String])
runContext ctx rules state = do
  let noState = runStateT ctx state
      noWriter = runWriterT noState
      noReader = runReaderT noWriter rules
  ((res, state'), log) <- noReader
  return (res, state', log)

-- | Evaluates a contextual action and returns just the result.
evalContext :: Monad m => ContextT s m a -> Rules -> s -> m a
evalContext ctx rules state = do
  (res, _, _) <- runContext ctx rules state
  return res

-- | Promotes a pure context to one able to do I/O.
promoteContext :: NFData s => PureContext s a -> ContextT s IO a
promoteContext ctx = do
  state <- get
  rules <- ask
  gen   <- liftIO getStdGen
  let (res, state', log, gen') = runPureContext ctx gen rules state
  liftIO $ setStdGen gen'
  put $!! state'
  tell $!! log
  return res

-- | Forces the evaluation of all parts of an I/O-based context.
forceContext :: NFData s => ContextT s IO ()
forceContext = do
  -- state
  s <- get
  liftIO $ evaluate (rnf s)

-- | Do a contextual action until it returns Nothing, making sure to always
-- fully evaluate the state and print out the contents of the log after
-- each iteration.
iterateContext :: NFData s => a -> (a -> ContextT s IO (Maybe a)) -> ContextT s IO ()
iterateContext val1 ctxf = do
  rules <- ask
  state1 <- get

  let loop state val = do
        (mval, state', log) <- runContext (ctxf val) rules state
        putStr (unlines log)
        evaluate (rnf state')
        case mval of
          Just val' -> loop state' val'
          Nothing   -> return ()

  liftIO $ loop state1 val1

whileContext :: NFData s => ContextT s IO Bool -> ContextT s IO ()
whileContext ctx =
  let ctxf _ = do
        continue <- ctx
        return $ if continue
                    then Just continue
                    else Nothing
  in iterateContext True ctxf
