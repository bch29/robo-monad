{-|
Module      : Game.Robo.Core.Rules
Description : Provides a default set of rules and lenses for Rules.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

This module makes the Rule lenses so that all rule lenses can be easily exported
in Game.Robo just by exporting the whole Game.Robo.Core.Rules module. This is
important because new rules are added all the time as part of ongoing
development and we want to restrict the number of places things need updating
every time a new rule is added.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax   #-}

module Game.Robo.Core.Rules where

import           Game.Robo.Core.Types
import           Lens.Micro.Platform

makeLenses ''Rules

-- | A sensible default set of rules to use in the simulation environment.
defaultRules ∷ Rules
defaultRules = Rules
  -- Robot
  { _ruleBotSize            = vec 60 40
  , _ruleMaxThrust          = 500
  , _ruleDriveFriction      = 0.97
  , _ruleMaxAngThrust       = 32
  , _ruleTurnFriction       = 0.85
  , _ruleMass               = 1
  , _ruleMaxEnergy          = 20
  , _ruleEnergyRechargeRate = 5

  -- Life bar
  , _ruleLifebarSize        = vec 60 4
  , _ruleLifebarOffset      = vec 0 (-50)
  , _ruleMaxLife            = 30

  -- Radar
  , _ruleRadarSize          = vec 10 30
  , _ruleMaxRadSpeed        = 16
  , _ruleRadRange           = 2000
  , _ruleRadFOV             = pi / 6

  -- Guns
  , _ruleGunSize            = vec 40 8
  , _ruleMaxGunTurnPower    = 64
  , _ruleGunFriction        = 0.85
  , _ruleBulletSpeed        = 600
  , _ruleMaxFirePower       = 4
  , _ruleMinFirePower       = 0.5

  -- Miscellaneous
  , _ruleArenaSize          = vec 800 800
  , _ruleSpawnMargin        = 100
  , _ruleMinSPS             = 12
  , _ruleMaxSPS             = 1000
  , _ruleDefaultSPS         = 60
  , _ruleTickSteps          = 6
  }
