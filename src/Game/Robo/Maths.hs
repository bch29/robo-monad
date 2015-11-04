{-|
Module      : Game.Robo.Maths
Description : Mathematical functions that are useful for programming robots.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

A set of mathematical functions that are both useful for programming robots and used
within the RoboMonad engine itself.

Organised into the (somewhat overlapping) categories of Vectors, Angles, Rects
and Other.

Note all angles are in radians unless specified otherwise.

-}

{-# LANGUAGE Trustworthy   #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Game.Robo.Maths
  (
  -- * Vectors
    angleTo, angleToHorizontal
  , vecFromAngle, rotateVec
  , vecMag, vecNorm
  , vecPerpR, vecPerpL
  , inSegment, inSegmentCentre

  -- * Angles
  , radToDeg, degToRad
  , angNormRelative, angNormAbsolute

  -- * Rects
  , withinRect, rectCorners, rectsIntersect
  , rectEnclosesRect, rectCornersOutside

  -- * Other
  , scanWall, getWallDist
  ) where

import           Game.Robo.Core.Types.Maths
import           Lens.Micro.Platform
-- import Linear.Vector

-----------------------------------
-- VECTORS
-----------------------------------

-- | The angle from the first argument to the second, in radians.
angleTo :: RealFloat a => V2 a -> V2 a -> a
angleTo (V2 x1 y1) (V2 x2 y2) = atan2 (y2 - y1) (x2 - x1)

-- | The (anticlockise) angle between the given vector and the x axis, in radians.
angleToHorizontal :: RealFloat a => V2 a -> a
angleToHorizontal (V2 x y) = atan2 y x

-- | Create a normalized vector in the direction of the given angle.
vecFromAngle :: Floating a => a -> V2 a
vecFromAngle ang = vec (cos ang) (sin ang)

rotateVec :: RealFloat a => a -> V2 a -> V2 a
rotateVec ang v = vecFromAngle newAng ^* vecMag v
  where oldAng = angleToHorizontal v
        newAng = oldAng + ang

-- | Get the magnitude of the vector.
vecMag :: Floating a => V2 a -> a
vecMag (V2 x y) = sqrt (x*x + y*y)

-- | Normalise a vector.
vecNorm :: Floating a => V2 a -> V2 a
vecNorm vec = vec ^* (1 / vecMag vec)

-- | The vector perpendicular to this one on the right-hand side.
vecPerpR :: Num a => V2 a -> V2 a
vecPerpR (V2 x y) = vec y (-x)

-- | The vector perpendicular to this one on the left-hand side.
vecPerpL :: Num a => V2 a -> V2 a
vecPerpL (V2 x y) = vec (-y) x

-- | Is a point within a segment of a circle with its centre at the origin?
-- The segment is the one obtained by rotating clockwise (positive angles)
-- from @ang1@ to @ang2@.
--
-- > inSegment ang1 ang2 radius xy
-- inSegment :: RealFloat a => a -> a -> a -> V2 a -> Bool
inSegment :: RealFloat a => a -> a -> a -> V2 a -> Bool
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
inSegmentCentre :: RealFloat a => a -> a -> a -> V2 a -> V2 a -> Bool
inSegmentCentre ang1 ang2 radius centre xy =
  inSegment ang1 ang2 radius (xy - centre)

-----------------------------------
-- ANGLES
-----------------------------------

-- | Convert an angle in radians to degrees.
radToDeg :: Floating c => c -> c
radToDeg = (/ pi) . (* 180)

-- | Convert an angle in degrees to radians.
degToRad :: Floating c => c -> c
degToRad = (* pi) . (/ 180)

-- | Normalise an angle to the range [-pi, pi)
angNormRelative :: (Floating a, Ord a) => a -> a
angNormRelative ang =
  case () of
    () | ang < -pi  -> angNormRelative (ang + 2 * pi)
       | ang >= pi   -> angNormRelative (ang - 2 * pi)
       | otherwise -> ang

-- | Normalise an angle to the range [0, 2*pi)
angNormAbsolute :: (Floating a, Ord a) => a -> a
angNormAbsolute ang =
  case () of
    () | ang < 0   -> angNormAbsolute (ang + 2 * pi)
       | ang >= 2*pi -> angNormAbsolute (ang - 2 * pi)
       | otherwise -> ang

-----------------------------------
-- RECTS
-----------------------------------

-- | Is a point within a rectangle?
withinRect :: Rect -> Vec -> Bool
withinRect rect vec = x >= 0 && y >= 0 && x <= maxX && y <= maxY
  where relVec    = vec - (rect^.rectCentre)
        rotated   = rotateVec (rect^.rectAngle) relVec
        (V2 x y) = rotated + 0.5 *^ (rect^.rectSize)
        (V2 maxX maxY) = rect^.rectSize

-- | Get all of the corners of the rectangle, in clockwise order.
rectCorners :: Rect -> [Vec]
rectCorners rect = (shift . rotate) basic
  where (V2 sx sy) = (rect^.rectSize) ^* 0.5
        basic = [ vec sx sy
                , vec sx (-sy)
                , vec (-sx) (-sy)
                , vec (-sx) sy ]
        rotate = map (rotateVec (rect^.rectAngle))
        shift  = map (+ rect^.rectCentre)

-- | Do there exist any points that are contained within both rectangles?
rectsIntersect :: Rect -> Rect -> Bool
rectsIntersect a b =
    any (withinRect a) (rectPoints b) ||
    any (withinRect b) (rectPoints a)

-- | Does 'Rect' @a@ encloses 'Rect' @b@ completely?
--
-- > rectEnclosesRect a b
rectEnclosesRect :: Rect -> Rect -> Bool
rectEnclosesRect a b = null (rectCornersOutside a b)

-- | Get the corners of @b@ that are outside @a@.
--
-- > rectCornersOutside a b
rectCornersOutside :: Rect -> Rect -> [Vec]
rectCornersOutside a b = filter (not . withinRect a) (rectCorners b)

-- | Returns the set of all corners of the 'Rect' and also its centre.
rectPoints :: Rect -> [Vec]
rectPoints rect = (rect^.rectCentre) : rectCorners rect

-----------------------------------
-- OTHER
-----------------------------------

-- | Get the coordinates of the nearest bit of wall that would be hit when scanning
-- from the robot's current position in the given direction.
--
-- > scanWall pos arenaSize dir
scanWall :: RealFloat a => V2 a -> V2 a -> V2 a -> V2 a
scanWall pos arenaSize dir =
  let V2 wx wy = arenaSize
      V2 dx dy = dir
      inY p = p >= 0 && p <= wy

      yLeft  = projectLeftY  pos dir
      xUp    = projectUpX    pos dir
      yRight = projectRightY pos arenaSize dir
      xDown  = projectDownX  pos arenaSize dir

      right = vec wx yRight
      left  = vec 0  yLeft
      down  = vec xDown wy
      up    = vec xUp   0
  in  case (signum dx, signum dy) of
        (s, 0)   -> if s > 0      then right else left
        (0, s)   -> if s > 0      then down  else up
        (1, 1)   -> if inY yRight then right else down
        (1, -1)  -> if inY yRight then right else up
        (-1, 1)  -> if inY yLeft  then left  else down
        (-1, -1) -> if inY yLeft  then left  else up
        _        -> error "scanWall"

-- | Get the euclidean distance to the nearest wall in the given vector direction.
--
-- > getWallDist pos arenaSize dir
getWallDist :: RealFloat a => V2 a -> V2 a -> V2 a -> a
getWallDist pos arenaSize dir =
  let xy = scanWall pos arenaSize dir
  in  vecMag (xy - pos)

{-
Helper functions for scanWall.
-}

-- | Get the Y coordinate of the projection of a point onto the left
-- wall along axis @dir@.
projectLeftY :: RealFloat a => V2 a -> V2 a -> a
projectLeftY (V2 x y) dir =
  let theta = pi - angleToHorizontal dir
  in  x * tan theta + y

-- | Get the X coordinate of the projection of a point onto the upper
-- wall along axis @dir@.
projectUpX :: RealFloat a => V2 a -> V2 a -> a
projectUpX (V2 x y) dir =
  let theta = angleToHorizontal dir - pi/2
  in  y * tan theta + x

-- | Get the Y coordinate of the projection of a point onto the right
-- wall along axis @dir@.
projectRightY :: RealFloat a => V2 a -> V2 a -> V2 a -> a
projectRightY (V2 x y) (V2 wx _) dir =
  let theta = angleToHorizontal dir
      dx = wx - x
  in  dx * tan theta + y

-- | Get the X coordinate of the projection of a point onto the lower
-- wall along axis @dir@.
projectDownX :: RealFloat a => V2 a -> V2 a -> V2 a -> a
projectDownX (V2 x y) (V2 _ wy) dir =
  let theta = pi/2 - angleToHorizontal dir
      dy = wy - y
  in  dy * tan theta + x
