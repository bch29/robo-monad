{-|
Module      : Game.Robo.PidController
Description : Typeclass-based PID controller implementation for use with robots.
Copyright   : (c) Bradley Hardy, 2015
License     : BSD3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable (depends on SDL)

-}

{-# LANGUAGE Trustworthy            #-} -- Enables compilation of robot files with Safe Haskell.
{-# LANGUAGE MultiParamTypeClasses  #-} -- Necessary for Pidable typeclass.
{-# LANGUAGE FunctionalDependencies #-} -- Necessary for Pidable typeclass.
{-# LANGUAGE TemplateHaskell        #-} -- Used to generate lenses for PidController.
module Game.Robo.PidController where

import Lens.Family2
import Lens.Family2.TH

import Game.Robo.Core.MathsTypes
import Game.Robo.Maths

data PidController a s =
     PidController { _pidGainP :: a
                   , _pidGainI :: a
                   , _pidGainD :: a
                   , _pidCutoffI :: a

                   , _pidTermI :: s
                   , _pidError :: s
                   , _pidOut   :: s
                   }

makeLenses ''PidController

-- | Make a PID controller given gains for the P, I and D terms respectively,
-- as well as a value for I cutoff.
makePid :: Pidable a s => a -> a -> a -> a -> PidController a s
makePid gp gi gd ci =
  PidController { _pidGainP = gp
                , _pidGainI = gi
                , _pidGainD = gd
                , _pidCutoffI = ci

                , _pidTermI = pidNone
                , _pidError = pidNone
                , _pidOut   = pidNone }

-- | Make a PID controller given gains for the P, I and D terms respectively,
-- using a sensible I cutoff.
makePidSimple :: Pidable a s => a -> a -> a -> PidController a s
makePidSimple gp gi gd = makePid gp gi gd 10

-- | Update a PID controller with a new error.
updatePid :: Pidable a s => s -> PidController a s -> PidController a s
updatePid newError pid =
    pid & pidError .~ newError
        & pidOut   .~ pterm `pidSum` dterm `pidSum` iterm
        & pidTermI .~ iterm
  where delta = pidDiff newError (pid^.pidError)
        pterm = mulScalar (pid^.pidGainP) newError
        dterm = mulScalar (pid^.pidGainD) delta
        iterm = if magnitude (pterm `pidSum` dterm) < (pid^.pidCutoffI)
                   then (pid^.pidTermI) `pidSum` (mulScalar (pid^.pidGainI) delta)
                   else pidNone

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
