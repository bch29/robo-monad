# RoboMonad
Heavily inspired by RoboCode. Program robots in Haskell and watch them fight.

Each robot runs in the Robo monad, which only allows access to a limited set of actions to control the robot and interact with the world around it.

## Running
Clone, then execute `cabal run` to see the demo. Uses the `SDL` and `SDL-gfx` packages, which may require installing the C SDL1.2 development libraries. See the Haddocks for documentation of Robo actions.

## Roadmap
 - Add health, let robots be destroyed
 - Make multiple rounds, keeping track of victories
 - Add collisions between robots
 - Allow the simulation at different speeds
 - Add a facility to allow robots to save data to files
