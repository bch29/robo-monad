{-|
Module      : Game.Robo.Extra
Description : Extra utility functions for programming robots.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable (depends on SDL)


-}

{-# LANGUAGE Trustworthy #-} -- Enables compilation of robot files with Safe Haskell.

module Game.Robo.Extra
  ( scanWallR, scanWallDirR
  , getWallDistR, getWallDistDirR
  ) where

import Control.Applicative

import Game.Robo
import Game.Robo.Maths

-- | Robo-monadic version of 'scanWall' that uses the robot's
-- current position but takes a direction parameter.
scanWallDirR :: Vec -> Robo s Vec
scanWallDirR dir = scanWall <$> getPosition <*> getRule ruleArenaSize <*> pure dir

-- | Robo-monadic version of 'getWallDist' that uses the robot's
-- current position but takes a direction parameter.
getWallDistDirR :: Vec -> Robo s Scalar
getWallDistDirR dir = getWallDist <$> getPosition <*> getRule ruleArenaSize <*> pure dir

-- | Robo-monadic version of 'scanWall' that uses the robot's
-- current position and direction.
scanWallR :: Robo s Vec
scanWallR = scanWallDirR =<< getHeadingVec

-- | Robo-monadic version of 'getWallDist' that uses the robot's
-- current position and direction.
getWallDistR :: Robo s Scalar
getWallDistR = getWallDistDirR =<< getHeadingVec


