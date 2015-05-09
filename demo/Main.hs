{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative

import Game.Robo
import Game.Robo.Maths
import Game.Robo.PidController

import Crazy
import TestBot

myRules :: BattleRules
myRules = defaultRules

main :: IO ()
main = runWorld myRules [testbot, crazy]
