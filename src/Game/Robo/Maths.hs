{-|
Module      : Game.Robo.Maths
Description : Mathematical functions that are useful for programming robots.
Copyright   : (c) Bradley Hardy, 2015
License     : BSD3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable (depends on SDL)

-}

{-# LANGUAGE Trustworthy #-} -- Enables compilation of robot files with Safe Haskell.

module Game.Robo.Maths
  -- vectors
  ( angleTo, angleToHorizontal
  , vecFromAngle, rotateVec
  , vecMag, vecNorm
  , vecPerpR, vecPerpL
  , inSegment, inSegmentCentre

  -- angles
  , radToDeg, degToRad
  , angNormRelative, angNormAbsolute

  -- rects
  , withinRect, rectCorners, rectsIntersect

  -- other
  , scanWall, getWallDist
  ) where

import Lens.Family2
import Game.Robo.Core.MathsTypes

-----------------------------------
-- VECTORS
-----------------------------------

-- | The angle from the first argument to the second, in radians.
angleTo :: Vec -> Vec -> Angle
angleTo (Vec x1 y1) (Vec x2 y2) = atan2 (y2 - y1) (x2 - x1)

-- | The (anticlockise) angle between the given vector and the x axis, in radians.
angleToHorizontal :: Vec -> Angle
angleToHorizontal (Vec x y) = atan2 y x

-- | Create a normalized vector in the direction of the given angle.
vecFromAngle :: Angle -> Vec
vecFromAngle ang = Vec (cos ang) (sin ang)

-- | Rotate a vector about the origin by an angle.
rotateVec :: Angle -> Vec -> Vec
rotateVec ang v = vecMag v *| vecFromAngle newAng
  where oldAng = angleToHorizontal v
        newAng = oldAng + ang

-- | Get the magnitude of the vector.
vecMag :: Vec -> Scalar
vecMag (Vec x y) = sqrt (x*x + y*y)

-- | Normalise a vector.
vecNorm :: Vec -> Vec
vecNorm vec = vec |* (1 / vecMag vec)

-- | The vector perpendicular to this one on the right-hand side.
vecPerpR :: Vec -> Vec
vecPerpR (Vec x y) = Vec y (-x)

-- | The vector perpendicular to this one on the left-hand side.
vecPerpL :: Vec -> Vec
vecPerpL (Vec x y) = Vec (-y) x

-- | Is a point within a segment of a circle with its centre at the origin?
-- The segment is the one obtained by rotating clockwise (positive angles)
-- from @ang1@ to @ang2@.
--
-- > inSegment ang1 ang2 radius xy
inSegment :: Angle -> Angle -> Scalar -> Vec -> Bool
inSegment ang1 ang2 radius xy = withinDist && withinAngles
  where withinDist = vecMag xy <= radius
        ang1' = angNormAbsolute ang1
        ang2' = angNormAbsolute ang2
        xyAng = angNormAbsolute (angleToHorizontal xy)
        withinAngles =
          (ang1' <= ang2' &&  xyAng >= ang1' && xyAng <= ang2') ||
          (ang1' >= ang2' && (xyAng >= ang1' || xyAng <= ang2'))

-- | Is a point within a segment of a circle with its centre at @centre@?
-- The segment is the one obtained by rotating clockwise (positive angles)
-- from @ang1@ to @ang2@.
--
-- > inSegmentCentre ang1 ang2 radius centre xy
inSegmentCentre :: Angle -> Angle -> Scalar -> Vec -> Vec -> Bool
inSegmentCentre ang1 ang2 radius centre xy =
  inSegment ang1 ang2 radius (xy - centre)

-----------------------------------
-- ANGLES
-----------------------------------

-- | Convert an angle in radians to degrees.
radToDeg :: Angle -> Angle
radToDeg = (/ pi) . (* 180)

-- | Convert an angle in degrees to radians.
degToRad :: Angle -> Angle
degToRad = (* pi) . (/ 180)

-- | Normalise an angle to the range [-pi, pi)
angNormRelative :: Angle -> Angle
angNormRelative ang =
  case () of
    () | ang < -pi  -> angNormRelative (ang + 2 * pi)
    () | ang >= pi   -> angNormRelative (ang - 2 * pi)
    () | otherwise -> ang

-- | Normalise an angle to the range [0, 2*pi)
angNormAbsolute :: Angle -> Angle
angNormAbsolute ang =
  case () of
    () | ang < 0   -> angNormAbsolute (ang + 2 * pi)
    () | ang >= 2*pi -> angNormAbsolute (ang - 2 * pi)
    () | otherwise -> ang

-----------------------------------
-- RECTS
-----------------------------------

-- | Is a point within a rectangle?
withinRect :: Vec -> Rect -> Bool
withinRect vec rect = x >= 0 && y >= 0 && x <= maxX && y <= maxY
  where relVec    = vec - (rect^.rectCentre)
        rotated   = rotateVec (rect^.rectAngle) relVec
        (Vec x y) = rotated + 0.5 *| (rect^.rectSize)
        (Vec maxX maxY) = rect^.rectSize

-- | Get all of the corners of the rectangle, in clockwise order.
rectCorners :: Rect -> [Vec]
rectCorners rect = (shift . rotate) basic
  where (Vec sx sy) = (rect^.rectSize) |* 0.5
        basic = [ Vec sx sy
                , Vec sx (-sy)
                , Vec (-sx) (-sy)
                , Vec (-sx) sy ]
        rotate = map (rotateVec (rect^.rectAngle))
        shift  = map (+ rect^.rectCentre)

-- | Do there exist any points that are contained within both rectangles?
rectsIntersect :: Rect -> Rect -> Bool
rectsIntersect a b =
    any (withinRect' a) (rectPoints b) ||
    any (withinRect' b) (rectPoints a)
  where withinRect' = flip withinRect
        rectPoints rect = (rect^.rectCentre) : rectCorners rect

-----------------------------------
-- OTHER
-----------------------------------

-- | Get the coordinates of the nearest bit of wall that would be hit when scanning
-- from the robot's current position in the given direction.
--
-- > scanWall pos arenaSize dir
scanWall :: Vec -> Vec -> Vec -> Vec
scanWall pos arenaSize dir =
  let Vec wx wy = arenaSize
      Vec  x  y = pos
      Vec dx dy = dir
      inX p = p >= 0 && p <= wx
      inY p = p >= 0 && p <= wy

      yLeft  = projectLeftY  pos dir
      xUp    = projectUpX    pos dir
      yRight = projectRightY pos arenaSize dir
      xDown  = projectDownX  pos arenaSize dir

      right = Vec wx yRight
      left  = Vec 0  yLeft
      down  = Vec xDown wy
      up    = Vec xUp   0
  in  case (signum dx, signum dy) of
        (s, 0)   -> if s > 0      then right else left
        (0, s)   -> if s > 0      then down  else up
        (1, 1)   -> if inY yRight then right else down
        (1, -1)  -> if inY yRight then right else up
        (-1, 1)  -> if inY yLeft  then left  else down
        (-1, -1) -> if inY yLeft  then left  else up

-- | Get the euclidean distance to the nearest wall in the given vector direction.
--
-- > getWallDist pos arenaSize dir
getWallDist :: Vec -> Vec -> Vec -> Scalar
getWallDist pos arenaSize dir =
  let xy = scanWall pos arenaSize dir
  in  vecMag (xy - pos)

{-
Helper functions for scanWall.
-}

-- | Get the Y coordinate of the projection of a point onto the left
-- wall along axis @dir@.
projectLeftY :: Vec -> Vec -> Scalar
projectLeftY (Vec x y) dir =
  let theta = pi - angleToHorizontal dir
  in  x * tan theta + y

-- | Get the X coordinate of the projection of a point onto the upper
-- wall along axis @dir@.
projectUpX :: Vec -> Vec -> Scalar
projectUpX (Vec x y) dir =
  let theta = angleToHorizontal dir - pi/2
  in  y * tan theta + x

-- | Get the Y coordinate of the projection of a point onto the right
-- wall along axis @dir@.
projectRightY :: Vec -> Vec -> Vec -> Scalar
projectRightY (Vec x y) (Vec wx _) dir =
  let theta = angleToHorizontal dir
      dx = wx - x
  in  dx * tan theta + y

-- | Get the X coordinate of the projection of a point onto the lower
-- wall along axis @dir@.
projectDownX :: Vec -> Vec -> Vec -> Scalar
projectDownX (Vec x y) (Vec _ wy) dir =
  let theta = pi/2 - angleToHorizontal dir
      dy = wy - y
  in  dy * tan theta + x
