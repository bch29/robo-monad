{-|
Module      : Game.Robo.Pidable
Description : Pidable typeclass -- a type that can be controlled with a PID controller.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable (depends on SDL)

-}

{-# LANGUAGE Trustworthy            #-} -- Enables compilation of robot files with Safe Haskell.
{-# LANGUAGE MultiParamTypeClasses  #-} -- Necessary for Pidable typeclass.
{-# LANGUAGE FunctionalDependencies #-} -- Necessary for Pidable typeclass.
module Game.Robo.PID.Class where

import Game.Robo.Core.MathsTypes
import Game.Robo.Maths

class (Fractional a, Ord a) => Pidable a s | s -> a where
  mulScalar :: a -> s -> s
  pidDiff   :: s -> s -> s
  pidSum    :: s -> s -> s
  magnitude :: s -> a
  pidNone   :: s

instance Pidable Double Double where
  mulScalar = (*)
  pidDiff   = (-)
  pidSum    = (+)
  magnitude = abs
  pidNone   = 0

instance Pidable Double Vec where
  mulScalar = (*|)
  pidDiff   = (-)
  pidSum    = (+)
  magnitude = vecMag
  pidNone   = 0
