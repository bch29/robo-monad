{-|
Module      : Game.Robo.Core.Lenses
Description : Lenses for relevant records.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

This is in a separate file to enable easy re-exporting of lenses
without having to go through and update export lists every time
we add a new field to a record.
-}

{-# LANGUAGE TemplateHaskell #-}

module Game.Robo.Core.Lenses where

import Game.Robo.Core.Types
import Lens.Micro.Platform

makeLenses ''WorldState

makeLenses ''BotState
makeLenses ''GunState
makeLenses ''RadarState

makeLenses ''Bullet
makeLenses ''BulletCollision
