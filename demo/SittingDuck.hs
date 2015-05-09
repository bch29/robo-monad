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
