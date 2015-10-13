{-|
Module      : Game.Robo.Core.World
Description : Handles the main simulation code.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE UnicodeSyntax             #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Game.Robo.Core.World (runWorld) where

import           Lens.Micro.Platform

import           Control.Concurrent

import           Control.Monad          hiding (mapM, mapM_)
import           Control.Monad.Random
import           Control.Monad.Reader   hiding (mapM, mapM_)
import           Control.Monad.State    hiding (mapM, mapM_)

import           Data.Array.IO

import           Data.Either
import           Data.Maybe
import           Data.Vector            (Vector)
import qualified Data.Vector            as V

import           Game.Robo.Core
import           Game.Robo.Core.Bot
import           Game.Robo.Render
import           Game.Robo.Render.World

type FullWorld m = (MonadIO m, MonadState WorldState m, MonadReader Rules m, MonadRandom m)

-- | Move a bullet along.
updateBullet :: Double -> Bullet -> Bullet
updateBullet passed bul = bul & bulPos %~ (+ (bul^.bulVel) |* passed)

-- | Check if a bullet is within the bounds of the arena.
isBulletInArena :: Vec -> Bullet -> Bool
isBulletInArena size bul =
    bx >= minX && bx <= maxX && by >= minY && by <= maxY
  where
    (minX, minY, maxX, maxY) = (0 :: Double, 0 :: Double, size^.vX, size^.vY)
    (bx, by) = (bul^.bulPos.vX, bul^.bulPos.vY)

-- | Moves all the bullets along, and removes them if they are outside
-- the arena bounds.
stepBullets
  :: (MonadState WorldState m, MonadReader Rules m) => Double -> m ()
stepBullets passed = do
  bullets <- use wldBullets
  size <- asks (view ruleArenaSize)
  let bullets' = V.filter (isBulletInArena size) . V.map (updateBullet passed) $ bullets
  wldBullets .= bullets'

vecPartitionEithers :: Vector (Either a b) -> (Vector a, Vector b)
vecPartitionEithers vec = (V.map getLeft  . V.filter isLeft  $ vec,
                           V.map getRight . V.filter isRight $ vec)
  where getRight (Right x) = x
        getRight _ = error "getRight on Left _"
        getLeft (Left x) = x
        getLeft _ = error "getLeft on Right _"

-- | Handles all bullet collisions, removing colliding bullets and returning
-- collision data.
handleBulletCollisions
  :: (MonadState WorldState m, MonadReader Rules m) =>
     m (Vector BulletCollision)
handleBulletCollisions = do
  let -- tests for a single bullet whether it has hit the bot in question
      test botState bul = (,) bul <$> testBulletHit botState bul
      maybeCollision bid (bul, True) =
        Right (BulletCollision (bul^.bulOwner) bid (bul^.bulPower))
      maybeCollision _ (bul, False) = Left bul
      checkForBot botState = do
        bullets <- use wldBullets
        hits <- mapM (test botState) bullets
        let (remainingBullets, collisions) =
              vecPartitionEithers .
              V.map (maybeCollision (botState^.botID)) $
              hits
        wldBullets .= remainingBullets
        return collisions
  botStates <- use wldBots
  collisions <- mapM checkForBot botStates
  return (join collisions)

-- | Removes dead bots from the game, updates other bots' IDs.
pruneBots :: (MonadState WorldState m) => m (Vector BotState)
pruneBots = do
  bots <- use wldBots
  chans <- use wldUpdateChans
  let ((aliveChans, aliveBots), (_, deadBots)) =
        V.partition ((> 0) . view botLife . snd) (V.zip chans bots) & each %~ V.unzip
  -- if there are dead bots, we need to get rid of them and assign new IDs to the rest
  if not (V.null deadBots) then do
    -- assign new bot IDs
    let updated = V.imap (set botID . (+1)) aliveBots
    wldBots .= updated
    wldUpdateChans .= aliveChans

    return deadBots
  else return mempty

-- | Steps the world (minus the bots) forward a tick, returns a list of
-- bullet collisions.
stepWorld
  :: (MonadState WorldState m, MonadReader Rules m)
    => Double -> m (Vector BulletCollision, Vector BotState)
stepWorld passed = do
  deadBots <- pruneBots
  stepBullets passed
  collisions <- handleBulletCollisions
  return (collisions, deadBots)

-- | Gets random positions within the given size for bots to start in.
generateSpawnPositions :: MonadRandom m ⇒ Int -> Scalar -> Vec -> m (Vector Vec)
generateSpawnPositions count margin size =
  V.replicateM count (getRandomR (1 |* margin, size - 1 |* (2*margin)))

-- | Collect the responses to a request from all the robots,
-- blocking until every robot has responded.
collectResponses :: Chan BotResponse -> Int -> IO (Vector BotResponse)
collectResponses chan numBots = do
    responses <- newArray (1, numBots) Nothing
      :: IO (IOArray BotID (Maybe BotResponse))
    collect numBots responses
    results <- getElems responses
    return . V.fromList . catMaybes $ results
  where
    collect 0 _ = return ()
    collect n responses = do
      response <- readChan chan
      writeArray responses (responseID response) (Just response)
      collect (n - 1) responses

-- | Collect responses and use them to update the world state.
updateWorldWithResponses :: FullWorld m => Chan BotResponse -> Int -> m ()
updateWorldWithResponses chan numBots = do
  responses <- liftIO $ collectResponses chan numBots
  let newBots = V.map responseState responses
      bullets = responseBullets =<< responses
  wldBots    .= newBots
  wldBullets %= (bullets `mappend`)

-- | Gets the current time in milliseconds.
getTime :: IO Int
getTime = fromIntegral <$> getTicks

-- | Change the SPS, making sure to clamp it to between the min and max.
setSPS :: (MonadState WorldState m, MonadReader Rules m) => Int -> m ()
setSPS newSPS = do
  minSPS <- asks (view ruleMinSPS)
  maxSPS <- asks (view ruleMaxSPS)
  let sps | newSPS < minSPS = minSPS
          | newSPS > maxSPS = maxSPS
          | otherwise       = newSPS

  time <- use wldTime
  wldTime0     .= time
  wldStepsDone .= 0
  wldSPS       .= sps

-- | Modify the SPS with a pure function, clamping between the min and max.
modifySPS :: (MonadState WorldState m, MonadReader Rules m) => (Int -> Int) -> m ()
modifySPS f = setSPS . f =<< use wldSPS

-- | The World's main logic action.
worldMain :: FullWorld m => m ()
worldMain = do
  -- get timing values
  time0     <- use wldTime0
  time      <- liftIO getTime
  stepsDone <- use wldStepsDone
  sps       <- use wldSPS

  -- update the stored time
  wldTime .= time

  -- only continue if we need to do more steps to catch up to where we should
  -- be
  let targetSteps = (time - time0) * sps `div` 1000
  when (stepsDone < targetSteps) $ do
    -- update the number of steps we have done
    wldStepsDone += 1

    -- work out whether we need to do a tick now
    wldSinceTick += 1
    sinceTick <- use wldSinceTick
    tickSteps <- asks (view ruleTickSteps)
    doTick <- if sinceTick >= tickSteps
                 then do wldSinceTick -= tickSteps
                         return True
                 else return False

    -- the time in seconds since the last step
    defSps <- asks (view ruleDefaultSPS)
    let passed = 1 / fromIntegral defSps

    -- update the world, getting back bullet collisions and dead bots
    (bulletCollisions, deadBots) <- stepWorld passed
    let wasBotInvolved bid col = col^.bcolAggressor == bid
                              || col^.bcolVictim    == bid

    -- kill the threads running dead bots
    liftIO $ mapM_ (maybe (return ()) killThread . view botTID) deadBots

    -- tell the bots to update themselves
    worldState <- get
    botStates  <- use wldBots
    let mkUpdate botState = BotUpdate
          { updateState  = botState
          , updateWorld  = worldState
          , updatePassed = passed
          , updateDoTick = doTick
          , updateBulletCollisions
            = V.filter (wasBotInvolved (botState^.botID)) bulletCollisions
          }
        botUpdates = V.map mkUpdate botStates

    -- get the channels out of the world state
    updateChans <- use wldUpdateChans
    Just responseChan <- use wldResponseChan
    liftIO $ V.zipWithM_ writeChan updateChans botUpdates

    -- get the responses back
    numBots <- V.length <$> use wldBots
    updateWorldWithResponses responseChan numBots

-- | Handle keyboard input.
worldKbd :: FullWorld m => Char -> m ()
worldKbd '+' = modifySPS (+10)
worldKbd '-' = modifySPS (subtract 10)
worldKbd '=' = do
  sps <- asks (view ruleDefaultSPS)
  setSPS sps
worldKbd _ = return ()

-- | Initialise the game.
worldInit :: FullWorld m => [BotSpec] -> m ()
worldInit specs = do
  -- initialise the bot states
  let numBots = length specs
  mass        <- asks (view ruleMass)
  life        <- asks (view ruleMaxLife)
  spawnMargin <- asks (view ruleSpawnMargin)
  arenaSize   <- asks (view ruleArenaSize)
  positions   <- generateSpawnPositions numBots spawnMargin arenaSize
  let bots = V.imap (initialBotState life mass . (+1)) positions
  wldBots .= bots

  -- initialise the channels
  updateChans  <- liftIO $ V.replicateM numBots newChan
  responseChan <- liftIO newChan

  -- start the bot threads, each with their own update channel
  rules <- ask
  let runBot' (spec, botState, updChan) =
        runBot rules spec botState updChan responseChan
  tids <- liftIO $ V.mapM (forkIO . runBot') (V.zip3 (V.fromList specs) bots updateChans)

  -- make sure the world knows what time it is (Robo time!)
  time <- liftIO getTime
  wldTime  .= time
  wldTime0 .= time

  -- get the responses to initialisation
  updateWorldWithResponses responseChan numBots
  -- assign bots their thread IDs
  wldBots %= V.zipWith (set botTID) (V.map Just tids)

  -- store the channels in the world state
  wldUpdateChans .= updateChans
  wldResponseChan .= Just responseChan

-- | Run a battle with the given rules and robots.
--
-- If you have 'BotSpec's @mybot1@, @mybot2@ and @mybot3@, then
-- you can start a RoboMonad simulation from your main function
-- like so:
--
-- > main = runWorld defaultRules [mybot1, mybot2, mybot3]
runWorld :: Rules -> [BotSpec] -> IO ()
runWorld rules specs = do
  -- work out the world dimensions
  let screenSize = rules^.ruleArenaSize
      (width, height) = (round (screenSize^.vX), round (screenSize^.vY)) :: (Int, Int)

  -- initialise the world state
  let _wldBots      = mempty
      _wldRect      = Rect (screenSize |* 0.5) screenSize 0
      _wldBullets   = mempty
      _wldTime0     = 0
      _wldTime      = 0
      _wldStepsDone = 0
      _wldSPS       = rules^.ruleDefaultSPS
      _wldSinceTick = 0
      _wldUpdateChans = mempty
      _wldResponseChan = Nothing

      actionInit = Just (worldInit specs)
      actionMain = Just worldMain
      actionDraw = Just drawWorld
      actionKeyboard = Just worldKbd

  -- start the game
  startGameLoop "RoboMonad" width height rules WorldState{..} GameActions{..}
