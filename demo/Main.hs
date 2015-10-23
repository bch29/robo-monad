{-# LANGUAGE UnicodeSyntax #-}
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

import           Game.Robo

-- Import the robots that we want to fight.
import           BulletTester
import           Crazy
import           ExampleBot
import           WallHugger

-- Use the default rules.
myRules ∷ Rules
myRules = defaultRules

-- Start the world with our ruleset and robots.
main ∷ IO ()
-- main = runWorld myRules ([crazy, crazy] >>= replicate 25)
main = runWorld myRules (replicate 50 bullettester)
-- main = runWorld myRules (replicate 50 examplebot)
