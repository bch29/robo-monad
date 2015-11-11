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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Game.Robo.Core
  ( runRobo
  , module X
  ) where

import           Control.Monad.Free.Church  (iterM)
import           Control.Monad.Random       (MonadRandom(
                                                 getRandom , getRandoms,
                                                 getRandomR, getRandomRs))
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict

import           Game.Robo.Core.Lenses      as X
import           Game.Robo.Core.Rules       as X
import           Game.Robo.Core.Types       as X

-- | Enables us to use another StateT while still being able
-- to access the underlying state.
newtype Wrapper m a =
  Wrapper { runWrapper :: m a }
  deriving (Functor      , Applicative  , Monad,
            MonadReader r, MonadWriter w, MonadRandom)

runRobo
  :: (StB m, Ru m, Ra m)
     => Robo s a -> s -> m (a, s)
runRobo (Robo robo) s =
  (runWrapper . flip runStateT s . iterM interpretRobo) robo

interpretRobo
  :: (StB m, Ru m, Ra m)
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
    -- LogR msg next ->
    --   do tell msg
    --      next
    LogR _ next -> next
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
