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
import Game.Robo.Draw.DrawWorld
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

updateWorldWithResponses :: ResponseChan -> Int -> IOWorld ()
updateWorldWithResponses chan numBots = do
  (newBots, bullets) <- liftIO $ collectResponses chan numBots
  wldBots    .= newBots
  wldBullets %= (bullets ++)

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
  size <- asks (view ruleArenaSize)
  wldBullets .= filter (isBulletInArena size) bullets'

-- | Handles all bullet collisions.
handleBulletCollisions :: World ()
handleBulletCollisions = do
    -- get the list of all bot IDs
    bids <- gets $ toListOf (wldBots.traverse.botID)
    handler bids
  where handler [] = return ()
        -- step through the bot ids
        handler (bid:bids) = do
          -- get all the bullets
          bullets <- use wldBullets
          -- see if the current bot is colliding with any of the bullets
          bulletsm <- applyBot bid $ mapM testBulletHit bullets
          -- remove the bullets that have collided
          wldBullets .= catMaybes bulletsm
          -- keep looping
          handler bids

-- | Steps the world (minus the bots) forward a tick.
stepWorld :: Double -> World ()
stepWorld passed = do
  stepBullets passed
  handleBulletCollisions

-- | Gets random positions within the given size for bots to start in.
generateSpawnPositions :: MonadRandom m => Int -> Scalar -> Vec -> m [Vec]
generateSpawnPositions count margin size =
  replicateM count (getRandomR (1 |* margin, size - 1 |* (2*margin)))

mainLoop :: Surface -> [UpdateChan] -> ResponseChan -> IOWorld ()
mainLoop surface updateChan responseChan = do
    time <- liftIO $ getTicksSeconds
    loop time 0
  where
    loop prevTime sinceTick = do
      -- Yield the CPU a little
      liftIO $ delay 0
      time <- liftIO $ getTicksSeconds
      let passed = time - prevTime

      -- we don't want to update too fast
      if passed >= 0.01 then do
        tickTime <- asks (view ruleTickTime)
        let (doTick, sinceTick') = if sinceTick > tickTime
                                      then (True, sinceTick - tickTime)
                                      else (False, sinceTick)

        -- tell the bots to update themselves
        state <- get
        let botStates = state^.wldBots
            botUpdates = zip4 botStates (repeat state) (repeat passed) (repeat doTick)
        liftIO $ zipWithM_ writeChan updateChan botUpdates

        -- get the responses back
        numBots <- length <$> use wldBots
        updateWorldWithResponses responseChan numBots

        -- do further world updates
        promoteContext (stepWorld passed)

        -- draw the world
        drawWorld surface
        liftIO (SDL.flip surface)

        -- loop back round unless we get an exit event
        event <- liftIO pollEvent
        unless (event == Quit) $ loop time (sinceTick' + passed)
      else loop prevTime sinceTick

worldMain :: Surface -> [BotSpec] -> IOWorld ()
worldMain surface specs = do
  -- initialise the bot states
  let numBots = length specs
  mass        <- asks (view ruleMass)
  spawnMargin <- asks (view ruleSpawnMargin)
  arenaSize   <- asks (view ruleArenaSize)
  positions   <- generateSpawnPositions numBots spawnMargin arenaSize
  let bots = zipWith (initialBotState mass) [1..] positions
  wldBots .= bots

  -- initialise the channels
  updateChans  <- liftIO $ replicateM numBots newChan
  responseChan <- liftIO $ newChan

  -- start the bot threads, each with their update channel
  rules <- ask
  let runBot' (spec, state, bid, updChan) = runBot rules spec state bid updChan responseChan
  liftIO $ mapM_ (forkIO . runBot') (zip4 specs bots [1..] updateChans)

  -- get the responses to initialisation
  updateWorldWithResponses responseChan numBots

  -- start the main loop
  mainLoop surface updateChans responseChan


-- | Run a battle with the given rules and robots.
runWorld :: BattleRules -> [BotSpec] -> IO ()
runWorld rules specs = withInit [InitEverything] $ do
  -- initialise libraries
  void TTF.init

  -- initialise the game
  -- make the window
  let screenSize = rules^.ruleArenaSize
      (width, height) = (round (screenSize^.vX), round (screenSize^.vY))
  screen <- setVideoMode width height 32 [SWSurface]

  -- initialise the world state
  let worldState =
        WorldState { _wldBots  = []
                   , _wldRect  = rect (screenSize |* 0.5) screenSize 0
                   , _wldBullets = []
                   }

  -- start the main loop
  evalIOContext (worldMain screen specs) rules worldState

  -- clean up
  TTF.quit
