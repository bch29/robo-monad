{-|
Module      : Game.Robo.Core.World
Description : Handles the main simulation code.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

module Game.Robo.Core.World (runWorld) where

import qualified Graphics.UI.GLUT as GL

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

import Control.DeepSeq

import Data.Array.MArray
import Data.Array.IO

import Data.List
import Data.Maybe

import Game.Robo.Core
import Game.Robo.Core.Bot
import Game.Robo.Render
import Game.Robo.Render.World

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

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = do
  res <- f x
  (as, bs) <- partitionM f xs
  -- abuse of list comprehension!
  return ([x | res] ++ as, [x | not res] ++ bs)

-- | Handles all bullet collisions, removing colliding bullets and returning
-- collision data.
handleBulletCollisions :: World [BulletCollision]
handleBulletCollisions = do
    -- get the list of all bot IDs
    bids <- gets $ toListOf (wldBots.traverse.botID)
    handler [] bids
  where handler acc [] = return acc
        -- step through the bot ids
        handler acc (bid:bids) = do
          -- get all the bullets
          bullets <- use wldBullets
          -- see if the current bot is colliding with any of the bullets
          (hitBullets, otherBullets) <- applyBot bid $ partitionM testBulletHit bullets
          -- remove the bullets that have collided
          wldBullets .= otherBullets
          -- indicate which bullets hit
          let bulResult bul = BulletCollision (bul^.bulOwner) bid (bul^.bulPower)
              res = map bulResult hitBullets
          -- keep looping
          handler (res ++ acc) bids

-- | Steps the world (minus the bots) forward a tick, returns a list of
-- bullet collisions.
stepWorld :: Double -> World [BulletCollision]
stepWorld passed = do
  stepBullets passed
  handleBulletCollisions

-- | Gets random positions within the given size for bots to start in.
generateSpawnPositions :: MonadRandom m => Int -> Scalar -> Vec -> m [Vec]
generateSpawnPositions count margin size =
  replicateM count (getRandomR (1 |* margin, size - 1 |* (2*margin)))

-- | Collect the responses to a request from all the robots,
-- blocking until every robot has responded.
collectResponses :: Chan BotResponse -> Int -> IO ([BotState], [Bullet])
collectResponses chan numBots = do
    responses <- newArray (1, numBots) Nothing :: IO (IOArray BotID (Maybe BotState))
    bullets   <- collect numBots [] responses
    results   <- getElems responses
    return $ (catMaybes results, bullets)
  where
    collect 0 bullets _ = return bullets
    collect n bullets responses = do
      (BotResponse bid bot buls) <- readChan chan
      writeArray responses bid (Just bot)
      collect (n - 1) (buls ++ bullets) responses

-- | Collect responses and use them to update the world state.
updateWorldWithResponses :: Chan BotResponse -> Int -> IOWorld ()
updateWorldWithResponses chan numBots = do
  (newBots, bullets) <- liftIO $ collectResponses chan numBots
  wldBots    .= newBots
  wldBullets %= (bullets ++)

-- | Gets the current time in milliseconds.
-- NB getTicks is from SDL and gives the time in milliseconds, unrelated to Robo ticks
getTime :: IO Int
getTime = fromIntegral <$> getTicks

-- | The World's main loop.
mainStep :: RenderData -> [Chan BotUpdate] -> Chan BotResponse -> IOWorld Bool
mainStep render updateChan responseChan = do
  -- Yield the CPU a little
  time      <- use wldTime
  time'     <- liftIO $ getTime
  stepIval  <- asks (view ruleStepInterval)

  -- add to the time since last step
  wldSinceStep += time' - time

  -- update the stored time
  wldTime .= time'

  -- only continue if enough time has passed since the last step
  sinceStep <- use wldSinceStep
  when (sinceStep >= stepIval) $ do
    -- subtract the step interval
    wldSinceStep -= stepIval

    wldSinceTick += 1
    sinceTick <- use wldSinceTick
    tickSteps <- asks (view ruleTickSteps)
    -- work out whether we need to do a tick now
    doTick <- if sinceTick >= tickSteps
                 then do wldSinceTick -= tickSteps
                         return True
                 else return False

    -- the time in seconds since the last step
    let passed = 1e-3 * fromIntegral stepIval
    -- update the world, getting back bullet collisions
    bulletCollisions <- promoteContext (stepWorld passed)
    let wasBotInvolved bid col = col^.bcolAggressor == bid || col^.bcolVictim == bid

    -- tell the bots to update themselves
    worldState <- get
    botStates <- use wldBots
    let mkUpdate botState = BotUpdate
          { updateState  = botState
          , updateWorld  = worldState
          , updatePassed = passed
          , updateDoTick = doTick
          , updateBulletCollisions
            = filter (wasBotInvolved (botState^.botID)) bulletCollisions
          }
        botUpdates = map mkUpdate botStates
    liftIO $ zipWithM_ writeChan updateChan botUpdates

    -- get the responses back
    numBots <- length <$> use wldBots
    updateWorldWithResponses responseChan numBots

    -- draw the world
    runDrawing drawWorld render
  return True

-- | Where the monads kick in.
worldMain :: RenderData -> [BotSpec] -> IOWorld ()
worldMain render specs = do
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

  -- start the bot threads, each with their own update channel
  rules <- ask
  let runBot' (spec, botState, bid, updChan) =
        runBot rules spec botState bid updChan responseChan
  liftIO $ mapM_ (forkIO . runBot') (zip4 specs bots [1..] updateChans)

  -- make sure the world knows what time it is (Robo time!)
  time <- liftIO getTime
  wldTime .= time

  -- get the responses to initialisation
  updateWorldWithResponses responseChan numBots

  -- start the main loop
  whileContext $ mainStep render updateChans responseChan

-- | Run a battle with the given rules and robots.
--
-- If you have 'BotSpec's @mybot1@, @mybot2@ and @mybot3@, then
-- you can start a RoboMonad simulation from your main function
-- like so:
--
-- > main = runWorld defaultRules [mybot1, mybot2, mybot3]
runWorld :: Rules -> [BotSpec] -> IO ()
runWorld rules specs = do
  -- make the window
  let screenSize = rules^.ruleArenaSize
      (width, height) = (round (screenSize^.vX), round (screenSize^.vY))
  render <- startRender "RoboMonad" width height

  -- initialise the world state
  let worldState = WorldState
        { _wldBots      = []
        , _wldRect      = Rect (screenSize |* 0.5) screenSize 0
        , _wldBullets   = []
        , _wldTime      = 0
        , _wldSinceStep = 0
        , _wldSinceTick = 0
        }

  -- jump into the World monad
  evalContext (worldMain render specs) rules worldState
  GL.mainLoop
