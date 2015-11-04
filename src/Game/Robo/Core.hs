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
  , runRoboContext
  , evalRoboContext
  , runPureRoboContext
  , forceRoboContext
  , iterateRoboContext
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
import           Data.IORef                  (IORef, readIORef, writeIORef)
import           Data.Maybe                  (fromMaybe)
import           Lens.Micro.Platform

import           Game.Robo.Core.Lenses       as X
import           Game.Robo.Core.Rules        as X
import           Game.Robo.Core.Types        as X

-- | Returns the first bot ID that satisfies a monadic predicate by running it
-- for each bot in turn but stopping when it returns True.
botsFindM :: Monad m => RoboContextT BotState m Bool -> RoboContextT WorldState m (Maybe BotID)
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
runPureRoboContext :: PureRoboContext s a -> StdGen -> Rules -> s -> (a, s, String, StdGen)
runPureRoboContext ctx gen rules st = (res, st', lg, gen')
  where noCtx = runRoboContext ctx rules st
        ((res, st', lg), gen') = runRand noCtx gen

-- | Runs a contextual action, returning the results, state and message log.
runRoboContext :: Monad m => RoboContextT s m a -> Rules -> s -> m (a, s, String)
runRoboContext ctx rules st = do
  let noState = runStateT ctx st
      noWriter = runWriterT noState
      noReader = runReaderT noWriter rules
  ((res, st'), lg) <- noReader
  return (res, st', lg)

-- | Evaluates a contextual action and returns just the result.
evalRoboContext :: Monad m => RoboContextT s m a -> Rules -> s -> m a
evalRoboContext ctx rules st = do
  (res, _, _) <- runRoboContext ctx rules st
  return res

-- | Forces the evaluation of all parts of an I/O-capable piece of state.
forceRoboContext :: (MonadState s m, MIO m, NFData s) => m ()
forceRoboContext = do
  s <- get
  liftIO $ evaluate (rnf s)

-- | Do a contextual action until it returns Nothing, making sure to always
-- fully evaluate the state and print out the contents of the log after
-- each iteration.
iterateRoboContext :: NFData s => a -> (a -> RoboContextT s IO (Maybe a)) -> RoboContextT s IO ()
iterateRoboContext val1 ctxf = do
  rules <- ask
  state1 <- get

  let loop st val = do
        (mval, st', lg) <- runRoboContext (ctxf val) rules st
        putStr lg
        evaluate (rnf st')
        case mval of
          Just val' -> loop st' val'
          Nothing   -> return ()

  liftIO $ loop state1 val1

-- | Runs a game action and updates the state reference when it is done.
runAction :: NFData s => Rules -> IORef s -> RoboContextT s IO () -> IO ()
runAction rules ref action = do
  st <- readIORef ref
  (_, st', lg) <- runRoboContext action rules st
  putStr lg
  evaluate (rnf st')
  writeIORef ref st'

-- | An initial, empty set of game actions.
gameActions :: GameActions s
gameActions = GameActions
  { actionInit     = Nothing
  , actionMain     = Nothing
  -- , actionDraw     = Nothing
  , actionKeyboard = Nothing
  }

printNum :: (MonadState s m, MIO m, Foldable t) => Getter s (t a) -> m ()
printNum l = do
  num <- length <$> use l
  liftIO (print num)

-- | Starts the game loop given a set of rules, an initial state,
-- and a set of actions.
startGameLoop :: Rules -- ^ The game rules.
              -> IORef WorldState -- ^ The reference to the world state
              -> IORef Bool -- ^ The reference which tells us when we should quit
              -> GameActions WorldState -- ^ The set of game actions.
              -> IO ()
startGameLoop rules ref quitRef GameActions{..} =
  evalRoboContext go rules =<< readIORef ref
  where
    maybeM = maybe (return ())
    go = do
      -- run the initialisation action if it exists
      fromMaybe (return ()) actionInit

      -- get the updated state
      st <- get

      -- update up our state reference
      liftIO $ writeIORef ref st

      -- let doKeyboard = fmap (\kbd _ k -> runAction rules ref (kbd k)) actionKeyboard
      -- liftIO $ setCharCallback (renderDataWin render) doKeyboard

      let doMain = maybeM (liftIO . runAction rules ref) actionMain
          mainLoop = do
            doMain
            st' <- liftIO (readIORef ref)
            put st'
            q <- liftIO (readIORef quitRef)
            unless q mainLoop
      mainLoop
