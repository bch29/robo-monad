{-|
Module      : Game.Robo.Pidable
Description : Pidable typeclass -- a type that can be controlled with a PID controller.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE Trustworthy            #-} -- Enables compilation of robot files with Safe Haskell.
{-# LANGUAGE MultiParamTypeClasses  #-} -- Necessary for Pidable typeclass.
{-# LANGUAGE FunctionalDependencies #-} -- Necessary for Pidable typeclass.
{-# LANGUAGE FlexibleInstances      #-} -- Necessary for Pidable GVec instance.

module Game.Robo.PID.Class where

import Game.Robo.Core.Types.Maths
import Game.Robo.Maths

class Fractional scalar => Pidable scalar val | val -> scalar where
  mulScalar :: scalar -> val -> val
  pidDiff   :: val -> val -> val
  pidSum    :: val -> val -> val
  magnitude :: val -> scalar
  pidZero   :: val

instance Pidable Double Double where
  mulScalar = (*)
  pidDiff   = (-)
  pidSum    = (+)
  magnitude = abs
  pidZero   = 0

instance Floating a => Pidable a (V2 a) where
  mulScalar = (*^)
  pidDiff   = (-)
  pidSum    = (+)
  magnitude = norm
  pidZero   = 0
