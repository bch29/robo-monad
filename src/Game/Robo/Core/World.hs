module Game.Robo.Core.World where

import Graphics.UI.SDL as SDL hiding (Rect)
import Graphics.UI.SDL.TTF as TTF

import Lens.Family2
import Lens.Family2.State

import Control.Concurrent

import Control.Applicative
import Data.Traversable (traverse)
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Random
import System.Random

import Data.Array.MArray
import Data.Array.IO

import Data.Vector.Class
import Data.List
import Data.Maybe

import Game.Robo.Core
import Game.Robo.Core.Bot
import Game.Robo.Core.DrawWorld
import Game.Robo.Maths

-- | Get the SDL ticks in seconds.
getTicksSeconds :: IO Double
getTicksSeconds = do
  ticks <- getTicks
  return $ 1e-3 * fromIntegral ticks

-- | Collect the responses to a request from all the robots,
-- blocking until every robot has responded.
collectResponses :: ResponseChan -> Int -> IO ([BotState], [Bullet])
collectResponses chan numBots = do
    responses <- newArray (1, numBots) Nothing :: IO (IOArray BotID (Maybe BotState))
    bullets <- collect numBots [] responses
    results <- getElems responses
    return $ (catMaybes results, bullets)
  where
    collect 0 bullets _ = return bullets
    collect n bullets responses = do
      (bid, bot, buls) <- readChan chan
      writeArray responses bid (Just bot)
      collect (n - 1) (buls ++ bullets) responses

updateWorldWithResponses :: ResponseChan -> Int -> WorldState -> IO WorldState
updateWorldWithResponses chan numBots state = do
  (botStates', bullets) <- collectResponses chan numBots
  let state' = state & wldBots .~ botStates'
                     & wldBullets %~ (bullets ++)
  return state'

-- | Move a bullet along.
updateBullet :: Double -> Bullet -> Bullet
updateBullet passed bul = bul & bulPos +~ (bul^.bulVel) |* passed

-- | Check if a bullet is within the bounds of the arena.
isBulletInArena :: Vec -> Bullet -> Bool
isBulletInArena size bul =
    bx >= minX && bx <= maxX && by >= minY && by <= maxY
  where
    (minX, minY, maxX, maxY) = (0, 0, size^.vX, size^.vY)
    (bx, by) = (bul^.bulPos.vX, bul^.bulPos.vY)

-- | Moves all the bullets along, and removes them if they are outside
-- the arena bounds.
stepBullets :: Double -> World ()
stepBullets passed = do
  bullets <- use wldBullets
  let bullets' = map (updateBullet passed) bullets
  size <- asks (view ruleWorldSize)
  wldBullets .= filter (isBulletInArena size) bullets'

-- | Handles all bullet collisions.
handleBulletCollisions :: StdGen -> World ()
handleBulletCollisions rgen = do
    -- get the list of all bot IDs
    bids <- gets $ toListOf (wldBots.traverse.botID)
    handler rgen bids
  where handler _ [] = return ()
        -- step through the bot ids
        handler gen (bid:bids) = do
          -- get all the bullets
          bullets <- use wldBullets
          -- see if the current bot is colliding with any of the bullets
          bulletsm <- applyBot gen bid $ mapM testBulletHit bullets
          -- remove the bullets that have collided
          wldBullets .= catMaybes bulletsm
          -- keep looping
          let (_, gen') = split gen
          handler gen' bids

-- | Steps the world (minus the bots) forward a tick.
stepWorld :: StdGen -> Double -> World ()
stepWorld gen passed = do
  stepBullets passed
  handleBulletCollisions gen

mainLoop :: Surface -> BattleRules -> WorldState -> [UpdateChan] -> ResponseChan -> IO ()
mainLoop surface rules worldState updateChan responseChan = do
    time <- getTicksSeconds
    loop worldState time 0
  where
    numBots = length (worldState^.wldBots)
    loop state prevTime sinceTick = do
      -- Yield the CPU a little
      delay 0

      time <- getTicksSeconds
      let passed = time - prevTime

      -- we don't want to update too fast
      if passed >= 0.01 then do
        let tickTime = rules^.ruleTickTime
            (doTick, sinceTick') = if sinceTick > tickTime
                                    then (True, sinceTick - tickTime)
                                    else (False, sinceTick)

        -- tell the bots to update themselves
        let botStates = state^.wldBots
            botUpdates = zip4 botStates (repeat state) (repeat passed) (repeat doTick)
        zipWithM_ writeChan updateChan botUpdates

        -- get the responses back
        state' <- updateWorldWithResponses responseChan numBots state

        -- do further world updates
        gen1 <- newStdGen
        gen2 <- newStdGen
        let (_, state'', log) = evalWorld (stepWorld gen2 passed) gen1 rules state'

        -- draw the world
        runDrawing (drawWorld surface) rules state''
        SDL.flip surface

        -- loop back round unless we get an exit event
        event <- pollEvent
        unless (event == Quit) $ loop state'' time (sinceTick' + passed)
      else loop state prevTime sinceTick

-- | Run a battle with the given rules and robots.
runWorld :: BattleRules -> [BotSpec] -> IO ()
runWorld rules specs = withInit [InitEverything] $ do
  -- initialise libraries
  void TTF.init

  -- initialise the game
  -- make the window
  let screenSize = rules^.ruleWorldSize
      (width, height) = (round (screenSize^.vX), round (screenSize^.vY))
  screen <- setVideoMode width height 32 [SWSurface]

  -- initialise the bot states
  let numBots = length specs
      xInterval = 800 / fromIntegral numBots
      y = 400
      mass = rules^.ruleMass
      firstX = 400 - xInterval * fromIntegral (numBots - 1) * 0.5
      positions = map (\i -> vec (firstX + fromIntegral i * xInterval) y) [0 .. numBots - 1]
      initialStates = zipWith (initialBotState mass) [1..] positions

  -- initialise the world state
  let worldState =
        WorldState { _wldBots  = initialStates
                   , _wldRect  = rect (screenSize |* 0.5) screenSize 0
                   , _wldBullets = []
                   }

  -- initialise the channels
  updateChans  <- replicateM numBots newChan
  responseChan <- newChan

  -- start the bot threads, each with their update channel
  let runBot' (spec, state, bid, updChan) = runBot rules spec state bid updChan responseChan
  mapM_ (forkIO . runBot') (zip4 specs initialStates [1..] updateChans)

  -- get the responses to initialisation
  worldState' <- updateWorldWithResponses responseChan numBots worldState

  -- start the main loop
  mainLoop screen rules worldState' updateChans responseChan

  -- clean up
  TTF.quit
