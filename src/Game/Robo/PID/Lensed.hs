{-|
Module      : Game.Robo.PID.Lensed
Description : Typeclass-based PID controller implementation for use with robots, using lenses.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable (depends on SDL)

-}

{-# LANGUAGE Trustworthy     #-} -- Enables compilation of robot files with Safe Haskell.
{-# LANGUAGE TemplateHaskell #-} -- Used to generate lenses for PidController.
module Game.Robo.PID.Lensed
  ( PidController (..)
  , pidGainP, pidGainI, pidGainD, pidCutoffI, pidTermI, pidError, pidOut
  , makePid, makePidSimple
  , updatePid
  , module Game.Robo.PID.Class
  ) where

import Lens.Family2
import Lens.Family2.TH

import Game.Robo.Maths

import Game.Robo.PID.Class

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
