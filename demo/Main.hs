{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative

import Game.Robo
import Game.Robo.Maths
import Game.Robo.PidController

import Crazy
import TestBot

myRules :: Rules
myRules = defaultRules

main :: IO ()
main = runWorld myRules [crazy, testbot]
