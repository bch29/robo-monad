{-|
Module      : Game.Robo.Core.MathsTypes
Description : Mathematical types and basic operations for 2D geometry.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable (depends on SDL)

Separated from Game.Robo.Core.Types so that the whole module
can be re-exported to the user. We make lenses here because
for the same reason.
-}

{-# LANGUAGE TemplateHaskell #-}

module Game.Robo.Core.MathsTypes where

import Lens.Family2.TH
import Control.DeepSeq
import Control.Monad.Random

---------------------------------
--  Core Types
---------------------------------

-- | A scalar quantity.
type Scalar = Double

-- | An angle in radians.
type Angle = Double

-- | A two-dimensional vector quantity.
data Vec = Vec
  { _vX :: !Scalar
  , _vY :: !Scalar
  }

-- | A rectangle with a centre, size and angle (to the horizontal).
data Rect = Rect
  { _rectCentre :: !Vec
  , _rectSize   :: !Vec
  , _rectAngle  :: !Angle
  }

---------------------------------
--  Basic functions on vectors
---------------------------------

instance Num Vec where
  (Vec a b) + (Vec x y) = Vec (a + x) (b + y)
  -- dot product seems the only sensible definition
  (Vec a b) * (Vec x y) = Vec (a * x) (b * y)
  negate (Vec x y) = Vec (-x) (-y)
  -- it would be nice if this could do vector magnitude, but it has to
  -- return another vector
  abs (Vec x y) = Vec (abs x) (abs y)
  signum (Vec x y) = Vec (signum x) (signum y)
  fromInteger i = Vec x x where x = fromInteger i

instance Eq Vec where
  Vec a b == Vec x y = a == x && b == y

-- This is only really used for @fromRational@.
instance Fractional Vec where
  (Vec a b) / (Vec x y) = Vec (a / x) (b / y)
  fromRational r = Vec x x where x = fromRational r

-- Functions to multiply vectors by scalars are very handy.

infixr 7 *|
infixl 7 |*

-- | Multiply a vector by a scalar on the left.
(*|) :: Scalar -> Vec -> Vec
a *| Vec x y = Vec (a * x) (a * y)

-- | Multiply a vector by a scalar on the right.
(|*) :: Vec -> Scalar -> Vec
Vec x y |* a = Vec (x * a) (y * a)

---------------------------------
--  Other Instances
---------------------------------

instance Show Vec where
  showsPrec prec (Vec x y) =
    showString "Vec " .
    showsPrec prec x .
    showString " " .
    showsPrec prec y

instance Show Rect where
  showsPrec prec (Rect cn sz ang) = showParen (prec /= 0) res
    where res = ("Rect " ++) . svec cn . spc . svec sz . spc . showsPrec (prec + 2) ang
          svec vec = showParen True $ showsPrec (prec + 1) vec
          spc = (' ':)

instance Random Vec where
  random gen = (Vec x y, gen'')
    where (x, gen')  = random gen
          (y, gen'') = random gen'

  randomR (Vec x1 y1, Vec x2 y2) gen = (Vec x y, gen'')
    where (x, gen')  = randomR (x1, x2) gen
          (y, gen'') = randomR (y1, y2) gen'

instance NFData Vec where
  rnf vec = vec `seq` ()

instance NFData Rect where
  rnf rect = rect `seq` ()

---------------------------------
--  Lenses
---------------------------------

makeLenses ''Vec
makeLenses ''Rect
