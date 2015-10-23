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
uses all of those.
-}

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE RecordWildCards            #-}

module Game.Robo.Core
  ( botsFindM
  , runRobo
  , runContext
  , evalContext
  , runPureContext
  , runDrawing
  , forceContext
  , iterateContext
  , gameActions
  , startGameLoop
  , printNum
  , module X
  ) where

import           Control.DeepSeq             (NFData (rnf))
import           Control.Exception           (evaluate)
import           Control.Monad.Free.Church   (iterM)
import           Control.Monad.Random        (MonadRandom (getRandom, getRandoms,
                                                           getRandomR, getRandomRs),
                                              StdGen, runRand)
import           Control.Monad.Reader        (MonadReader (ask),
                                              MonadTrans (lift), lift, liftIO,
                                              runReaderT, unless)
import           Control.Monad.State.Strict  (MonadState (get, put),
                                              StateT (runStateT))
import           Control.Monad.Writer.Strict (MonadWriter (tell), runWriterT)
import           Data.IORef                  (IORef, newIORef, readIORef,
                                              writeIORef)
import           Data.Maybe                  (fromMaybe)
import           Graphics.UI.GLFW            (pollEvents, setCharCallback,
                                              windowShouldClose)
import           Lens.Micro.Platform

import           Game.Robo.Core.Lenses       as X
import           Game.Robo.Core.Rules        as X
import           Game.Robo.Core.Types        as X
import           Game.Robo.Render

-- | Returns the first bot ID that satisfies a monadic predicate by running it
-- for each bot in turn but stopping when it returns True.
botsFindM :: Monad m => ContextT BotState m Bool -> ContextT WorldState m (Maybe BotID)
botsFindM action = do
  botStates <- use wldBots
  let doWhile bid sts' (st:sts) = do
        (done, st') <- lift $ runStateT action st
        if not done
           then doWhile (bid+1) (st':sts') sts
           else return (Just bid, reverse (st':sts') ++ sts)
      doWhile _ sts' [] = return (Nothing, reverse sts')
  (res, newStates) <- doWhile 1 [] (manyToList botStates)
  wldBots .= manyFromList newStates
  return res

-- | Enables us to use another StateT while still being able
-- to access the underlying state.
newtype Wrapper m a =
  Wrapper { runWrapper :: m a }
  deriving (Functor      , Applicative  , Monad,
            MonadReader r, MonadWriter w, MonadRandom)

runRobo
  :: (Wr m, StB m, Ru m, Ra m)
     => Robo s a -> s -> m (a, s)
runRobo (Robo robo) s =
  (runWrapper . flip runStateT s . iterM interpretRobo) robo

interpretRobo
  :: (Wr m, StB m, Ru m, Ra m)
     => RoboF s (StateT s (Wrapper m) b) -> StateT s (Wrapper m) b
interpretRobo robo =
  case robo of
    GetUStateR f ->
      do s <- get
         f s
    PutUStateR s next ->
      do put s
         next
    GetIStateR f ->
      do s <- lift2 get
         f s
    PutIStateR s next ->
      do lift2 (put s)
         next
    RulesR f ->
      do rules <- ask
         f rules
    LogR msg next ->
      do tell msg
         next
    RandVR f ->
      do r <- getRandom
         f r
    RandIR range f ->
      do r <- getRandomR range
         f r
    RandsVR f ->
      do rs <- getRandoms
         f rs
    RandsIR range f ->
      do rs <- getRandomRs range
         f rs
  where lift2 = lift . Wrapper


-- | Runs an action in a stateful context, returning the resulting state,
-- random number generator and message log along with the result of the action.
runPureContext :: PureContext s a -> StdGen -> Rules -> s -> (a, s, String, StdGen)
runPureContext ctx gen rules st = (res, st', lg, gen')
  where noCtx = runContext ctx rules st
        ((res, st', lg), gen') = runRand noCtx gen

-- | Runs a contextual action, returning the results, state and message log.
runContext :: Monad m => ContextT s m a -> Rules -> s -> m (a, s, String)
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

-- | Does a DrawContext's drawing.
runDrawing
  :: (MonadState s m, MIO m, Ru m)
     => DrawContext s ()
     -> RenderData -> m ()
runDrawing action render = do
  rules <- ask
  theState <- get
  liftIO $ runDraw (runReaderT action (rules, theState)) render

-- | Forces the evaluation of all parts of an I/O-capable piece of state.
forceContext :: (MonadState s m, MIO m, NFData s) => m ()
forceContext = do
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
        putStr lg
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
  putStr lg
  evaluate (rnf st')
  writeIORef ref st'

-- | An initial, empty set of game actions.
gameActions :: GameActions s
gameActions = GameActions
  { actionInit     = Nothing
  , actionMain     = Nothing
  , actionDraw     = Nothing
  , actionKeyboard = Nothing
  }

printNum :: (MonadState s m, MIO m, Foldable t) => Getter s (t a) -> m ()
printNum l = do
  num <- length <$> use l
  liftIO (print num)

-- | Starts the game loop given a set of rules, an initial state,
-- and a set of actions.
startGameLoop :: (NFData s) =>
                 String -- ^ The window name.
              -> Int -- ^ The window width.
              -> Int -- ^ The window height.
              -> Rules -- ^ The game rules.
              -> s -- ^ The initial game state.
              -> GameActions s -- ^ The set of game actions.
              -> IO ()
startGameLoop winName winW winH rules initialState GameActions{..} =
  evalContext go rules initialState
  where
    doMaybeM = maybe (return ())
    go = do
      -- initialise
      render <- liftIO $ startRender winName winW winH

      -- run the initialisation action if it exists
      fromMaybe (return ()) actionInit

      -- get the state
      st <- get

      -- set up our state reference
      ref <- liftIO $ newIORef st

      let doKeyboard = fmap (\kbd _ k -> runAction rules ref (kbd k)) actionKeyboard
      liftIO $ setCharCallback (renderDataWin render) doKeyboard

      let doMain    = doMaybeM (liftIO . runAction rules ref) actionMain
          doDrawing = doMaybeM (`runDrawing` render)          actionDraw
          mainLoop lastTick = do
            liftIO pollEvents
            doMain
            st' <- liftIO (readIORef ref)
            put st'
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
