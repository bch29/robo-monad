module Main where

import Game.Robo

import WallHugger
import Crazy

myRules :: Rules
myRules = defaultRules

main :: IO ()
main = runWorld myRules [crazy, wallhugger]
