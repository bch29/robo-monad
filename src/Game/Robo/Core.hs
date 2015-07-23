{-|
Module      : Game.Robo.Core
Description : Core operations shared by lots of parts of the game engine.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

Mainly contains utility functions for unwrapping/lifting/promoting/handling monads,
re-exports Types, Lenses and Rules because almost everything that uses Core also
use all of those.
-}

module Game.Robo.Core
  ( applyBot
  , applyBots
  , applyAllBots
  , botsFindM
  , runRobo
  , runContext
  , evalContext
  , runPureContext
  , promoteContext
  , runDrawing
  , forceContext
  , iterateContext
  , gameActions
  , startGameLoop
  , module X
  ) where

import Graphics.UI.GLFW
import Data.IORef
import Data.Array
import Data.Maybe (fromMaybe)

import Lens.Family2.State

import Control.DeepSeq
import Control.Exception

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Random
import Control.Applicative

import Game.Robo.Core.Types as X
import Game.Robo.Core.Rules as X
import Game.Robo.Core.Lenses as X
import Game.Robo.Render

-- | Evaluate a Bot monadic action in the context of its world.
applyBot :: Monad m => BotID -> ContextT BotState m a -> ContextT WorldState m a
applyBot bid bot = do
  botStates <- use wldBots
  let (prevStates, targetState : postStates) = splitAt (bid - 1) botStates
  (result, newState) <- lift $ runStateT bot targetState
  wldBots .= prevStates ++ newState : postStates
  return result

-- | Evaluate many Bot monadic actions in the context of their world.
-- Much more efficient than running applyBot for every Bot.
applyBots :: Monad m => [(BotID, ContextT BotState m a)] -> ContextT WorldState m [a]
applyBots bots = do
  botStates <- use wldBots
  let initialArr = listArray (1, length botStates) botStates
      mkNewState (bid, action) = do
        (res, st) <- lift $ runStateT action (initialArr ! bid)
        return (res, (bid, st))
  newStates <- mapM mkNewState bots
  let (res, updates) = unzip newStates
      finalArr = initialArr // updates
  wldBots .= elems finalArr
  return res

-- | Evaluate each given Bot action for each robot in the world in turn.
applyAllBots :: Monad m => [ContextT BotState m a] -> ContextT WorldState m [a]
applyAllBots bots = do
  botStates <- use wldBots
  let runfun action st = lift $ runStateT action st
  (res, newStates) <- unzip <$> zipWithM runfun bots botStates
  wldBots .= newStates
  return res

-- | Return the first bot ID that satisfies a monadic predicate by running it
-- for each bot in turn but stopping when it returns True.
botsFindM :: Monad m => ContextT BotState m Bool -> ContextT WorldState m (Maybe BotID)
botsFindM action = do
  botStates <- use wldBots
  let doWhile bid sts' (st:sts) = do
        (cont, st') <- lift $ runStateT action st
        if cont
           then doWhile (bid+1) (st':sts') sts
           else return (Just bid, reverse (st':sts') ++ sts)
      doWhile _ sts' [] = return (Nothing, reverse sts')
  (res, newStates) <- doWhile 1 [] botStates
  wldBots .= newStates
  return res

-- | Evaluate a Robo down to its underlying Bot monad.
runRobo :: Robo s a -> s -> Bot (a, s)
runRobo (Robo ctrl) s = runBotWrapper (runStateT ctrl s)

-- | Runs an action in a stateful context, returning the resulting state,
-- random number generator and message log along with the result of the action.
runPureContext :: PureContext s a -> StdGen -> Rules -> s -> (a, s, [String], StdGen)
runPureContext ctx gen rules st = (res, st', lg, gen')
  where noCtx = runContext ctx rules st
        ((res, st', lg), gen') = runRand noCtx gen

-- | Runs a contextual action, returning the results, state and message log.
runContext :: Monad m => ContextT s m a -> Rules -> s -> m (a, s, [String])
runContext ctx rules st = do
  let noState = runStateT ctx st
      noWriter = runWriterT noState
      noReader = runReaderT noWriter rules
  ((res, st'), lg) <- noReader
  return (res, st', lg)

-- | Evaluates a contextual action and returns just the result.
evalContext :: Monad m => ContextT s m a -> Rules -> s -> m a
evalContext ctx rules st = do
  (res, _, _) <- runContext ctx rules st
  return res

-- | Promotes a pure context to one able to do I/O.
promoteContext :: NFData s => PureContext s a -> ContextT s IO a
promoteContext ctx = do
  st <- get
  rules <- ask
  gen   <- liftIO getStdGen
  let (res, st', lg, gen') = runPureContext ctx gen rules st
  liftIO $ setStdGen gen'
  put $!! st'
  tell $!! lg
  return res

-- | Does a DrawContext's drawing within an IO Context.
runDrawing :: NFData s => DrawContext s a -> RenderData -> ContextT s IO a
runDrawing ctx render = do
  st <- get
  rules <- ask :: ContextT s IO Rules
  gen   <- liftIO getStdGen
  let rand = runContext ctx rules st
      draw = runRandT rand gen
  ((res, st', lg), gen') <- liftIO $ runDraw draw render
  liftIO $ setStdGen gen'
  put $!! st'
  tell $!! lg
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

  let loop st val = do
        (mval, st', lg) <- runContext (ctxf val) rules st
        putStr (unlines lg)
        evaluate (rnf st')
        case mval of
          Just val' -> loop st' val'
          Nothing   -> return ()

  liftIO $ loop state1 val1

-- | Runs a game action and updates the state reference when it is done.
runAction :: NFData s => Rules -> IORef s -> ContextT s IO () -> IO ()
runAction rules ref action = do
  st <- readIORef ref
  (_, st', lg) <- runContext action rules st
  putStr (unlines lg)
  evaluate (rnf st')
  writeIORef ref st'

-- | An initial, empty set of game actions.
gameActions = GameActions
  { actionInit     = Nothing
  , actionMain     = Nothing
  , actionDraw     = Nothing
  , actionKeyboard = Nothing
  }

-- | Starts the game loop given a set of rules, an initial state,
-- and a set of actions.
startGameLoop :: NFData s =>
                 String -- ^ The window name.
              -> Int -- ^ The window width.
              -> Int -- ^ The window height.
              -> Rules -- ^ The game rules.
              -> s -- ^ The initial game state.
              -> GameActions s -- ^ The set of game actions.
              -> IO ()
startGameLoop winName winW winH rules initialState actions =
  evalContext go rules initialState
  where
    go = do
      -- initialise
      render <- liftIO $ startRender winName winW winH

      -- run the initialisation action if it exists
      fromMaybe (return()) (actionInit actions)

      -- get the state
      st <- get

      -- set up our state reference
      ref <- liftIO $ newIORef st

      let doMain = case actionMain actions of
            Just action -> liftIO $ runAction rules ref action
            Nothing -> return ()

          doKeyboard = case actionKeyboard actions of
            Just kbd -> Just $ \_ k -> runAction rules ref $ kbd k
            Nothing -> Nothing

          doDrawing =
            case actionDraw actions of
              Just draw -> liftIO $ runAction rules ref (runDrawing draw render)
              Nothing -> return ()

      liftIO $ setCharCallback (renderDataWin render) doKeyboard

      let mainLoop lastTick = do
            liftIO pollEvents
            doMain
            ticks <- liftIO getTicks
            let tickTime = 1000 `div` 60
            lastTick' <- if ticks - lastTick > tickTime
                            then do doDrawing
                                    liftIO (drawRender render)
                                    return (lastTick + tickTime)
                            else return lastTick
            q <- liftIO $ windowShouldClose (renderDataWin render)
            unless q (mainLoop lastTick')
      mainLoop 0
