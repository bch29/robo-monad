{-|
Module      : Game.Robo.Core.Types.Maths
Description : Mathematical types and basic operations for 2D geometry.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

Separated from Game.Robo.Core.Types so that the whole module
can be re-exported to the user. We make lenses here
for the same reason.
-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.Robo.Core.Types.Maths where

import           Control.Applicative  (liftA, liftA2)
import           Control.DeepSeq
import           Control.Monad.Random
import           Data.Foldable        (fold)
import           Lens.Micro.Platform

---------------------------------
--  Core Types
---------------------------------

-- | A scalar quantity.
type Scalar = Double

-- | An angle in radians.
type Angle = Double

-- | A two-dimensional vector quantity.
data GVec a = Vec
  { _vX :: !a
  , _vY :: !a
  }
  deriving (Eq, Ord)

type Vec = GVec Scalar

-- | A rectangle with a centre, size and angle (to the horizontal).
data Rect = Rect
  { _rectCentre :: !Vec
  , _rectSize   :: !Vec
  , _rectAngle  :: !Angle
  }

---------------------------------
--  Basic functions on vectors
---------------------------------

instance Num a => Num (GVec a) where
  (+) = liftA2 (+)
  -- dot product seems the only sensible definition
  (*) = liftA2 (*)
  negate = fmap negate
  -- it would be nice if this could do vector magnitude, but it has to
  -- return another vector
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

-- This is only really used for @fromRational@.
instance Fractional a => Fractional (GVec a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational

-- Functions to multiply vectors by scalars are very handy.

infixr 7 *|
infixl 7 |*

-- | Multiply a vector by a scalar on the left.
(*|) :: Num a => a -> GVec a -> GVec a
(*|) a = fmap (a *)

-- | Multiply a vector by a scalar on the right.
(|*) :: Num a => GVec a -> a -> GVec a
(|*) = flip (*|)

---------------------------------
--  Other Instances
---------------------------------

instance Functor GVec where
  fmap = liftA

instance Applicative GVec where
  pure x = Vec x x
  Vec f g <*> Vec x y = Vec (f x) (g y)

instance Foldable GVec where
  foldMap f (Vec x y) = f x `mappend` f y

instance Traversable GVec where
  traverse f (Vec x y) = Vec <$> f x <*> f y

instance Show a => Show (GVec a) where
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

instance Random a => Random (GVec a) where
  random gen = (Vec x y, gen'')
    where (x, gen')  = random gen
          (y, gen'') = random gen'

  randomR (Vec x1 y1, Vec x2 y2) gen = (Vec x y, gen'')
    where (x, gen')  = randomR (x1, x2) gen
          (y, gen'') = randomR (y1, y2) gen'

instance NFData a => NFData (GVec a) where
  rnf v = v `seq` (fold . fmap rnf) v

instance NFData Rect where
  rnf r@(Rect p1 p2 a) = r `seq` rnf p1 `seq` rnf p2 `seq` rnf a

---------------------------------
--  Lenses
---------------------------------

makeLenses ''GVec
makeLenses ''Rect
