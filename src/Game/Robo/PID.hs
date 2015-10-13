{-|
Module      : Game.Robo.PID
Description : Typeclass-based PID controller implementation for use with robots without using lenses.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Robo.PID
  ( PID (..)
  , makePid, makePidSimple
  , updatePid
  , module Game.Robo.PID.Class
  ) where

import           Game.Robo.PID.Class

data PID scalar val = PID
  { pidGainP   :: scalar
  , pidGainI   :: scalar
  , pidGainD   :: scalar
  , pidCutoffI :: scalar

  , pidTermI   :: val
  , pidError   :: val
  , pidOut     :: val
  }

-- | Make a PID controller given gains for the P, I and D terms respectively,
-- as well as a value for I cutoff.
makePid :: Pidable scalar val => scalar -> scalar -> scalar -> scalar -> PID scalar val
makePid gp gi gd ci = PID
  { pidGainP = gp
  , pidGainI = gi
  , pidGainD = gd
  , pidCutoffI = ci

  , pidTermI = pidZero
  , pidError = pidZero
  , pidOut   = pidZero }

-- | Make a PID controller given gains for the P, I and D terms respectively,
-- using a sensible I cutoff.
makePidSimple :: Pidable scalar val => scalar -> scalar -> scalar -> PID scalar val
makePidSimple gp gi gd = makePid gp gi gd 10

-- | Update a PID controller with a new error.
updatePid :: (Pidable scalar val, Ord scalar) => val -> PID scalar val -> PID scalar val
updatePid newError pid =
    pid { pidError = newError
        , pidOut   = pterm `pidSum` dterm `pidSum` iterm
        , pidTermI = iterm }
  where delta = pidDiff newError (pidError pid)
        pterm = mulScalar (pidGainP pid) newError
        dterm = mulScalar (pidGainD pid) delta
        iterm = if magnitude (pterm `pidSum` dterm) < pidCutoffI pid
                   then pidTermI pid `pidSum` mulScalar (pidGainI pid) delta
                   else pidZero
