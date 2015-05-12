{-|
Module      : SittingDuck
Description : A robot that just sits there and does nothing.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable (depends on SDL)

-}

{-# LANGUAGE TemplateHaskell #-}

module SittingDuck (sittingDuck) where

import Game.Robo
import Game.Robo.Maths
import Game.Robo.PidController

type SittingDuck = Robo SittingDuckState

data SittingDuckState = SittingDuckState
     { _someState :: Int
     }

makeLenses ''SittingDuckState

emptyState :: SittingDuckState
emptyState = SittingDuckState
  { _someState = 0
  }

myInit :: SittingDuck ()
myInit = do
  return ()

myTick :: SittingDuck ()
myTick = do
  return ()

myScan :: ScanData -> SittingDuck ()
myScan s = do
  return ()

sittingDuck :: BotSpec
sittingDuck = BotSpec
  { botName = "sittingDuck"
  , botInitialState = emptyState
  , onInit = myInit
  , onTick = myTick
  , onScan = myScan
  }
