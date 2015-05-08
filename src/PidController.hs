{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TemplateHaskell #-}
module PidController where

import Lens.Family2
import Lens.Family2.TH

import Types
import Maths
import Data.Vector.Class
import Data.Vector.V2

data PidController a s =
     PidController { _pidGainP :: a
                   , _pidGainI :: a
                   , _pidGainD :: a

                   , _pidTermI :: s
                   , _pidError :: s
                   , _pidOut   :: s
                   }

makeLenses ''PidController

makePid :: Pidable a s => a -> a -> a -> PidController a s
makePid gp gi gd =
  PidController { _pidGainP = gp
                , _pidGainI = gi
                , _pidGainD = gd

                , _pidTermI = pidNone
                , _pidError = pidNone
                , _pidOut   = pidNone }

updatePid :: Pidable a s => s -> PidController a s -> PidController a s
updatePid newError pid =
    pid & pidError .~ newError
        & pidOut   .~ pterm `pidSum` dterm `pidSum` iterm
  where delta = pidDiff newError (pid^.pidError)
        pterm = mulScalar (pid^.pidGainP) newError
        dterm = mulScalar (pid^.pidGainD) delta
        iterm = pid^.pidTermI

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
