{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TemplateHaskell #-}
module Game.Robo.PidController where

import Lens.Family2
import Lens.Family2.TH

import Game.Robo.Core
import Game.Robo.Maths
import Data.Vector.Class
import Data.Vector.V2

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

makePid :: Pidable a s => a -> a -> a -> a -> PidController a s
makePid gp gi gd ci =
  PidController { _pidGainP = gp
                , _pidGainI = gi
                , _pidGainD = gd
                , _pidCutoffI = ci

                , _pidTermI = pidNone
                , _pidError = pidNone
                , _pidOut   = pidNone }

makePidSimple :: Pidable a s => a -> a -> a -> PidController a s
makePidSimple gp gi gd = makePid gp gi gd 10

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

instance Pidable Double Vector2 where
  mulScalar = (*|)
  pidDiff   = (-)
  pidSum    = (+)
  magnitude = vmag
  pidNone   = 0
