{-|
Module      : Game.Robo.Core.World
Description : Handles the main simulation code.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Game.Robo.Core.World (runWorld) where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Array.IO
import           Data.IORef
import           Data.Maybe
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import           Data.Time.Calendar          (Day (..))
import           Data.Time.Clock
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW
import           Lens.Micro.Platform

import           Game.Robo.Core
import           Game.Robo.Core.Bot
import           Game.Robo.Core.Collision
import           Game.Robo.Maths
import           Game.Robo.Render
import           Game.Robo.Render.World

-- | Move a bullet along.
updateBullet :: Double -> Bullet -> Bullet
updateBullet passed bul = bul & bulPos %~ (+ (bul^.bulVel) ^* passed)

-- | Check if a bullet is within the bounds of the arena.
isBulletInArena :: Vec -> Bullet -> Bool
isBulletInArena size bul =
    bx >= minX && bx <= maxX && by >= minY && by <= maxY
  where
    (minX, minY, maxX, maxY) = (0 :: Double, 0 :: Double, size^._x, size^._y)
    (bx, by) = (bul^.bulPos._x, bul^.bulPos._y)

-- | Moves all the bullets along, and removes them if they are outside
-- the arena bounds.
stepBullets
  :: (StW m, Ru m) => Double -> m ()
stepBullets passed = do
  size <- view ruleArenaSize
  bullets <- use wldBullets
  let stepped = updateBullet passed <$> bullets
      filteredBuls = filterMany (isBulletInArena size) stepped
  wldBullets .= filteredBuls

listToSet :: Ord a => Many a -> Set a
listToSet = foldr S.insert mempty

listFromSet :: Ord a => Set a -> Many a
listFromSet = manyFromList . S.toList

-- | Handles all bullet collisions, removing colliding bullets and returning
-- collision data.
handleBulletCollisions
  :: (StW m, Ru m) =>
     m (Many BulletCollision)
handleBulletCollisions = do
  minSize <- vecMag <$> view ruleBotSize
  arenaSize <- view ruleArenaSize
  bullets <- use wldBullets
  let entities = pointEntity (view bulPos) <$> bullets
      grid = entityGrid minSize arenaSize (listToVector entities)

      checkForBot botState = do
        rect <- botRect botState
        let collidedBullets = findCollisions rect grid
            collisions = mkCollision (view botID botState) <$> listFromVector collidedBullets
        return collisions

      mkCollision bid bul = (bul, BulletCollision (bul^.bulOwner) bid (bul^.bulPower))

  botStates <- use wldBots
  (collidedBullets, collisions) <- unzipMany . join <$> mapM checkForBot botStates

  allBullets <- use wldBullets
  let collidedBulletSet = listToSet collidedBullets
      allBulletSet = listToSet allBullets
      remainingBulletsSet = allBulletSet S.\\ collidedBulletSet

      remainingBullets = listFromSet remainingBulletsSet

  wldBullets .= remainingBullets

  return collisions

-- | Removes dead bots from the game, updates other bots' IDs.
pruneBots :: StW m => m (Many BotState)
pruneBots = do
  bots <- use wldBots
  chans <- use wldUpdateChans
  let ((aliveChans, aliveBots), (_, deadBots)) =
        partitionMany ((> 0) . view botLife . snd) (zipMany chans bots) & each %~ unzipMany
  -- if there are dead bots, we need to get rid of them and assign new IDs to the rest
  if not (null deadBots) then do
    -- assign new bot IDs
    let updated = imapMany (set botID . (+1)) aliveBots
    wldBots .= updated
    wldUpdateChans .= aliveChans

    return deadBots
  else return mempty

-- | Steps the world (minus the bots) forward a tick, returns a list of
-- bullet collisions.
stepWorld
  :: (StW m, Ru m)
    => Double -> m (Many BulletCollision, Many BotState)
stepWorld passed = do
  deadBots <- pruneBots
  stepBullets passed
  collisions <- handleBulletCollisions
  return (collisions, deadBots)
  -- return ([], deadBots)

-- | Gets random positions within the given size for bots to start in.
generateSpawnPositions :: Ra m => Int -> Scalar -> Vec -> m (Many Vec)
generateSpawnPositions count margin size =
  replicateManyM count (getRandomR (1 ^* margin, size - 1 ^* (2*margin)))

-- | Collect the responses to a request from all the robots,
-- blocking until every robot has responded.
collectResponses :: Chan BotResponse -> Int -> IO (Many BotResponse)
collectResponses chan numBots = do
    responses <- newArray (1, numBots) Nothing
      :: IO (IOArray BotID (Maybe BotResponse))

    let collect 0 = return ()
        collect n = do
          response <- readChan chan
          writeArray responses (responseID response) (Just response)
          collect (n - 1)

    collect numBots

    results <- getElems responses
    return . manyFromList . catMaybes $ results

-- | Collect responses and use them to update the world state.
updateWorldWithResponses :: (StW m, Ru m, MIO m) => Chan BotResponse -> Int -> m ()
updateWorldWithResponses chan numBots = do
  responses <- liftIO $ collectResponses chan numBots
  let newBots = fmap responseState responses
      bullets = responseBullets =<< responses
  wldBots    .= newBots
  wldBullets %= (bullets `mappend`)

-- | Gets the current time in milliseconds.
getTime :: IO Int
getTime = round
        .  (* 1000)
        .  flip diffUTCTime (UTCTime (ModifiedJulianDay 0) 0)
       <$> getCurrentTime

-- | Change the SPS, making sure to clamp it to between the min and max.
setSPS :: (StW m, Ru m) => Int -> m ()
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
modifySPS :: (StW m, Ru m) => (Int -> Int) -> m ()
modifySPS f = setSPS . f =<< use wldSPS

-- | The World's main logic action.
worldMain :: (StW m, Ru m, Ra m, MIO m) => m ()
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
    let mkUpdate botState = worldState `deepseq` BotUpdate
          { updateState  = botState
          , updateWorld  = worldState
          , updatePassed = passed
          , updateDoTick = doTick
          , updateBulletCollisions
            = filterMany (wasBotInvolved (botState^.botID)) bulletCollisions
          }
        botUpdates = fmap mkUpdate botStates

    -- get the channels out of the world state
    updateChans <- use wldUpdateChans
    Just responseChan <- use wldResponseChan
    liftIO $ zipWithManyM_ writeChan updateChans botUpdates

    -- get the responses back
    numBots <- length <$> use wldBots
    updateWorldWithResponses responseChan numBots
    return ()

-- | Handle keyboard input.
worldKbd :: (StW m, Ru m, MIO m) => Char -> m ()
worldKbd '+' = modifySPS (+10)
worldKbd '-' = modifySPS (subtract 10)
worldKbd '=' = do
  sps <- asks (view ruleDefaultSPS)
  setSPS sps
worldKbd _ = return ()

-- | Initialise the game.
worldInit :: (StW m, Ru m, MIO m, Ra m) => [BotSpec] -> m ()
worldInit specs = do
  -- initialise the bot states
  let numBots = length specs
  mass        <- asks (view ruleMass)
  life        <- asks (view ruleMaxLife)
  spawnMargin <- asks (view ruleSpawnMargin)
  arenaSize   <- asks (view ruleArenaSize)
  positions   <- generateSpawnPositions numBots spawnMargin arenaSize
  let bots = imapMany (initialBotState life mass . (+1)) positions
  wldBots .= bots

  -- initialise the channels
  updateChans  <- liftIO $ replicateManyM numBots newChan
  responseChan <- liftIO newChan

  -- start the bot threads, each with their own update channel
  rules <- ask
  let runBot' (spec, botState, updChan) =
        runBot rules spec botState updChan responseChan
  tids <- liftIO $ mapManyM (forkIO . runBot') (zipMany3 (manyFromList specs) bots updateChans)

  -- make sure the world knows what time it is (Robo time!)
  time <- liftIO getTime
  wldTime  .= time
  wldTime0 .= time

  -- get the responses to initialisation
  updateWorldWithResponses responseChan numBots
  -- assign bots their thread IDs
  wldBots %= zipManyWith (set botTID) (fmap Just tids)

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
      (width, height) = (round (screenSize^._x), round (screenSize^._y)) :: (Int, Int)
      winConf = WindowConf width height "RoboMonad"

  -- initialise the world state
  let _wldBots      = mempty
      _wldRect      = Rect (screenSize ^* 0.5) screenSize 0
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
      actionKeyboard = Just worldKbd

  stateRef <- newIORef WorldState{..}

  -- start the rendering thread
  (renderTid, quitRef) <-
    runRendering
    winConf
    (\loop ->
       do initData <- initRenderWorld rules
          shader <- compileShader (worldShader rules)
          loop (renderWorld rules stateRef initData shader))

  -- start the game loop
  startGameLoop rules stateRef quitRef GameActions{..}

  -- finally kill the rendering thread
  killThread renderTid
