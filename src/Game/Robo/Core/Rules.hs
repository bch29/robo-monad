{-# LANGUAGE TemplateHaskell #-}
module Game.Robo.Core.Rules where

import Lens.Family2.TH
import Game.Robo.Core.Types

defaultRules :: Rules
defaultRules = Rules
  { _ruleMaxThrust          = 500
  , _ruleMaxAngThrust       = 32
  , _ruleMaxGunTurnPower    = 64
  , _ruleMaxRadSpeed        = 16
  , _ruleMaxFirePower       = 4
  , _ruleMinFirePower       = 0.5
  , _ruleMass               = 1
  , _ruleDriveFriction      = 0.97
  , _ruleTurnFriction       = 0.85
  , _ruleGunFriction        = 0.85
  , _ruleMaxEnergy          = 20
  , _ruleEnergyRechargeRate = 5
  , _ruleBotSize            = vec 60 40
  , _ruleGunSize            = vec 40 8
  , _ruleRadarSize          = vec 10 30
  , _ruleBulletSpeed        = 400
  , _ruleRadRange           = 2000
  , _ruleRadFOV             = pi / 6
  , _ruleArenaSize          = vec 800 800
  , _ruleSpawnMargin        = 100
  , _ruleStepInterval       = round (1000 / 60)
  , _ruleTickSteps          = 6
  }


makeLenses ''Rules
