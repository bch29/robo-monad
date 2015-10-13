{-|
Module      : Game.Robo.PID.Lensed
Description : Typeclass-based PID controller implementation for use with robots, using lenses.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE Trustworthy     #-} -- Enables compilation of robot files with Safe Haskell.
{-# LANGUAGE TemplateHaskell #-} -- Used to generate lenses for PID.
module Game.Robo.PID.Lensed
  ( PID (..)
  , pidGainP, pidGainI, pidGainD, pidCutoffI, pidTermI, pidError, pidOut
  , makePid, makePidSimple
  , updatePid
  , module Game.Robo.PID.Class
  ) where

import Lens.Micro.Platform

import Game.Robo.PID.Class

data PID scalar val = PID
  { _pidGainP :: scalar
  , _pidGainI :: scalar
  , _pidGainD :: scalar
  , _pidCutoffI :: scalar

  , _pidTermI :: val
  , _pidError :: val
  , _pidOut   :: val
  }

makeLenses ''PID

-- | Make a PID controller given gains for the P, I and D terms respectively,
-- as well as a value for I cutoff.
makePid :: Pidable scalar val => scalar -> scalar -> scalar -> scalar -> PID scalar val
makePid gp gi gd ci = PID
  { _pidGainP = gp
  , _pidGainI = gi
  , _pidGainD = gd
  , _pidCutoffI = ci

  , _pidTermI = pidZero
  , _pidError = pidZero
  , _pidOut   = pidZero }

-- | Make a PID controller given gains for the P, I and D terms respectively,
-- using a sensible I cutoff.
makePidSimple :: Pidable scalar val => scalar -> scalar -> scalar -> PID scalar val
makePidSimple gp gi gd = makePid gp gi gd 10

-- | Update a PID controller with a new error.
updatePid :: (Pidable scalar val, Ord scalar) => val -> PID scalar val -> PID scalar val
updatePid newError pid =
    pid & pidError .~ newError
        & pidOut   .~ pterm `pidSum` dterm `pidSum` iterm
        & pidTermI .~ iterm
  where delta = pidDiff newError (pid^.pidError)
        pterm = mulScalar (pid^.pidGainP) newError
        dterm = mulScalar (pid^.pidGainD) delta
        iterm = if magnitude (pterm `pidSum` dterm) < (pid^.pidCutoffI)
                then (pid^.pidTermI) `pidSum` mulScalar (pid^.pidGainI) delta
                else pidZero
