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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Robo.Core.Types.Maths
       ( Scalar
       , Angle
       , Vec
       , Rect (..), rectAngle, rectCentre, rectSize
       , vec

       , module L
       )
       where

import           Control.DeepSeq
import           Control.Monad.Random
import           Lens.Micro.Platform
import           Linear.V2 as L
import           Linear.Vector as L
import           Linear.Metric as L

---------------------------------
--  Core Types
---------------------------------

-- | A scalar quantity.
type Scalar = Double

-- | An angle in radians.
type Angle = Double

-- | A two-dimensional vector quantity.
type Vec = V2 Scalar

-- | A rectangle with a centre, size and angle (to the horizontal).
data Rect = Rect
  { _rectCentre :: !Vec
  , _rectSize   :: !Vec
  , _rectAngle  :: !Angle
  }

-- | Build a vector out of two scalars.
vec :: a -> a -> V2 a
vec = V2

---------------------------------
--  Instances
---------------------------------

instance Show Rect where
  showsPrec prec (Rect cn sz ang) = showParen (prec /= 0) res
    where res = ("Rect " ++) . svec cn . spc . svec sz . spc . showsPrec (prec + 2) ang
          svec v = showParen True $ showsPrec (prec + 1) v
          spc = (' ':)

-- | Orphan instance!
instance Random a => Random (V2 a) where
  random gen = (V2 x y, gen'')
    where (x, gen')  = random gen
          (y, gen'') = random gen'

  randomR (V2 x1 y1, V2 x2 y2) gen = (V2 x y, gen'')
    where (x, gen')  = randomR (x1, x2) gen
          (y, gen'') = randomR (y1, y2) gen'

instance NFData Rect where
  rnf r@(Rect p1 p2 a) = r `seq` rnf p1 `seq` rnf p2 `seq` rnf a

---------------------------------
--  Lenses
---------------------------------

makeLenses ''Rect
