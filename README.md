# RoboMonad
Heavily inspired by RoboCode. Program robots in Haskell and watch them fight.

Each robot runs in the Robo monad, which only allows access to a limited set of actions to control the robot and interact with the world around it.

## Running
Clone, then execute `cabal run` to see the demo. See the Haddocks for documentation of Robo actions.

## Controls
For now there are very few in-game controls:
 - '+' to increase the simulation speed (by 10 steps per second)
 - '-' to decrease the simulation speed (by 10 steps per second)

## Roadmap
 - Make multiple rounds, keeping track of victories
 - Add collisions between robots
 - Add a facility to allow robots to save data to files
