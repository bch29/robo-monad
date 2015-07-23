{-|
Module      : Main
Description : An example showing how to start RoboMonad.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

module Main where

import Game.Robo

-- Import the robots that we want to fight.
import WallHugger
import Crazy
import BulletTester

-- Use the default rules.
myRules :: Rules
myRules = defaultRules

-- Start the world with our ruleset and robots.
main :: IO ()
main = runWorld myRules [crazy, wallhugger]
-- main = runWorld myRules (replicate 80 wallhugger)
