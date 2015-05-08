module Maths where

import Lens.Family2
import Data.Vector.V2
import Data.Vector.Class

import Types

-----------------------------------
-- VECTORS
-----------------------------------

-- | The angle from the first argument to the second, in radians.
angleTo :: Vec -> Vec -> Angle
angleTo (Vector2 x1 y1) (Vector2 x2 y2) = atan2 (y2 - y1) (x2 - x1)

-- | The (anticlockise) angle between the given vector and the x axis, in radians.
angleToHorizontal :: Vec -> Angle
angleToHorizontal (Vector2 x y) = atan2 y x

-- | Create a normalized vector in the direction of the given angle.
vecFromAngle :: Angle -> Vec
vecFromAngle ang = vec (cos ang) (sin ang)

-- | Rotate a vector about the origin by an angle.
rotateVec :: Angle -> Vec -> Vec
rotateVec ang vec = vmag vec *| vecFromAngle newAng
  where oldAng = angleToHorizontal vec
        newAng = oldAng + ang

-----------------------------------
-- ANGLES
-----------------------------------

-- | Convert an angle in radians to degrees.
radToDeg :: Angle -> Angle
radToDeg = (/ pi) . (* 180)

-- | Convert an angle in degrees to radians.
degToRad :: Angle -> Angle
degToRad = (* pi) . (/ 180)

-- | Normalise an angle to the range (-pi, pi]
normaliseAngle :: Angle -> Angle
normaliseAngle ang =
  case () of
    () | ang <= -pi  -> normaliseAngle (ang + 2 * pi)
    () | ang > pi   -> normaliseAngle (ang - 2 * pi)
    () | otherwise -> ang

-----------------------------------
-- RECTS
-----------------------------------

-- | Is a point within a rectangle?
withinRect :: Vec -> Rect -> Bool
withinRect vec rect = x >= 0 && y >= 0 && x <= maxX && y <= maxY
  where relVec    = vec - (rect^.rectCentre)
        rotated   = rotateVec (rect^.rectAngle) relVec
        (Vector2 x y) = rotated + 0.5 *| (rect^.rectSize)
        (Vector2 maxX maxY) = rect^.rectSize

-- | Get all of the corners of the rectangle, in clockwise order.
rectCorners :: Rect -> [Vec]
rectCorners rect = (shift . rotate) basic
  where (Vector2 sx sy) = (rect^.rectSize) |* 0.5
        basic = [ vec sx sy
                , vec sx (-sy)
                , vec (-sx) (-sy)
                , vec (-sx) sy ]
        rotate = map (rotateVec (rect^.rectAngle))
        shift  = map (+ rect^.rectCentre)

-- | Do there exist any points that are contained within both rectangles?
rectsIntersect :: Rect -> Rect -> Bool
rectsIntersect a b =
    any (withinRect' a) (rectPoints b) ||
    any (withinRect' b) (rectPoints a)
  where withinRect' = flip withinRect
        rectPoints rect = (rect^.rectCentre) : rectCorners rect
