module Game.Robo.Extra (scanWall, getWallDist) where

import Control.Applicative

import Game.Robo
import Game.Robo.Maths

-- | Get the coordinates of the nearest bit of wall that would be hit when scanning
-- from the robot's current position in the given direction.
scanWall :: Vec -> Robo s Vec
scanWall dir = do
  (wx, wy) <- vecPair <$> getRule ruleArenaSize
  (x, y)   <- vecPair <$> getPosition
  let (dx, dy) = vecPair dir
      inX p = p >= 0 && p <= wx
      inY p = p >= 0 && p <= wy

  yRight <- projectRightY dir
  yLeft  <- projectLeftY  dir
  xDown  <- projectDownX  dir
  xUp    <- projectUpX    dir

  let right = vec wx yRight
      left  = vec 0  yLeft
      down  = vec xDown wy
      up    = vec xUp   0

  return $ case (signum dx, signum dy) of
    (s, 0)   -> if s > 0      then right else left
    (0, s)   -> if s > 0      then down  else up
    (1, 1)   -> if inY yRight then right else down
    (1, -1)  -> if inY yRight then right else up
    (-1, 1)  -> if inY yLeft  then left  else down
    (-1, -1) -> if inY yLeft  then left  else up

-- | Get the euclidean distance to the nearest wall in the given vector direction.
getWallDist :: Vec -> Robo s Scalar
getWallDist dir = do
  xy <- scanWall dir
  pos <- getPosition
  return $ vecMag (xy - pos)

-- | Helper functions for scanWall.
projectRightY :: Vec -> Robo s Scalar
projectRightY dir = do
  (x, y)  <- vecPair <$> getPosition
  (wx, _) <- vecPair <$> getRule ruleArenaSize
  let theta = angleToHorizontal dir
      dx = wx - x
      -- projected y coordinate
      py = dx * tan theta + y
  return py

projectLeftY :: Vec -> Robo s Scalar
projectLeftY dir = do
  (x, y) <- vecPair <$> getPosition
  let theta = pi - angleToHorizontal dir
      -- projected y coordinate
      py = x * tan theta + y
  return py

projectUpX :: Vec -> Robo s Scalar
projectUpX dir = do
  (x, y) <- vecPair <$> getPosition
  let theta = angleToHorizontal dir - pi/2
      -- projected x coordinate
      px = y * tan theta + x
  return px

projectDownX :: Vec -> Robo s Scalar
projectDownX dir = do
  (x, y)  <- vecPair <$> getPosition
  (_, wy) <- vecPair <$> getRule ruleArenaSize
  let theta = pi/2 - angleToHorizontal dir
      dy = wy - y
      -- projected x coordinate
      px = dy * tan theta + x
  return px

