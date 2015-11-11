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
{-# LANGUAGE ScopedTypeVariables       #-}

module Game.Robo.Core.World (runWorld) where

import           Control.Concurrent                hiding (yield)
import           Control.Concurrent.Async
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Array.IO
import           Data.Maybe                        (catMaybes)
import           Data.Set                          (Set)
import qualified Data.Set                          as S
import           Data.Time.Calendar                (Day (..))
import           Data.Time.Clock
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW       (WindowConf(..))
import           Graphics.GPipe.Context.GLFW.Input (windowShouldClose)
import           Lens.Micro.Platform
import           Pipes                             hiding (each)
import           Pipes.Concurrent
import           System.Exit                       (exitSuccess)

import           Game.Robo.Core
import           Game.Robo.Core.Bot
import           Game.Robo.Core.Collision
import           Game.Robo.Core.Input
import           Game.Robo.Render
import           Game.Robo.Render.World

data Event
  = IncSPS
  | DecSPS
  | ResetSPS
  | Quit
  | Step Double -- The time since the last step
         Bool -- Should we do a bot tick or not?

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
  minSize <- norm <$> view ruleBotSize
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
  outputs <- use wldBotOutputs
  let ((aliveOutputs, aliveBots), (_, deadBots)) =
        partitionMany ((> 0) . view botLife . snd) (zipMany outputs bots) & each %~ unzipMany
  -- if there are dead bots, we need to get rid of them and assign new IDs to the rest
  if not (null deadBots) then do
    -- assign new bot IDs
    let updated = imapMany (set botID . (+1)) aliveBots
    wldBots .= updated
    wldBotOutputs .= aliveOutputs

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

-- | Gets random positions within the given size for bots to start in.
generateSpawnPositions :: Ra m => Int -> Scalar -> Vec -> m (Many Vec)
generateSpawnPositions count margin size =
  replicateManyM count (getRandomR (1 ^* margin, size - 1 ^* (2*margin)))

-- | Collect the responses to a request from all the robots,
-- blocking until every robot has responded.
collectResponses :: Input BotResponse -> Int -> IO (Many BotResponse)
collectResponses input numBots = do
  responses <- newArray (1, numBots) Nothing
    :: IO (IOArray BotID (Maybe BotResponse))

  let collect 0 = return ()
      collect n = do
        response <- atomically (recv input)
        case response of
          Just resp -> writeArray responses (responseID resp) response
          Nothing -> return ()
        collect (n - 1)

  collect numBots

  results <- getElems responses
  return . manyFromList . catMaybes $ results

-- | Collect responses and use them to update the world state.
updateWorldWithResponses :: (StW m, Ru m, MIO m) => m ()
updateWorldWithResponses = do
  Just input <- use wldBotInput
  numBots <- length <$> use wldBots
  responses <- liftIO (collectResponses input numBots)

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
setSPS :: (StW m, Ru m, MIO m) => Int -> m ()
setSPS newSPS = do
  minSPS <- asks (view ruleMinSPS)
  maxSPS <- asks (view ruleMaxSPS)
  let sps | newSPS < minSPS = minSPS
          | newSPS > maxSPS = maxSPS
          | otherwise       = newSPS

  spsVar <- use wldSPSVar
  liftIO $ putMVar spsVar sps
  wldSPS .= sps

-- | Modify the SPS with a pure function, clamping between the min and max.
modifySPS :: (StW m, Ru m, MIO m) => (Int -> Int) -> m ()
modifySPS f = setSPS . f =<< use wldSPS

-- | The World's main logic action.
worldMain :: (StW m, Ru m, Ra m, MIO m) => Event -> m ()
worldMain (Step passed doTick) = do
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
    updateOutputs <- use wldBotOutputs
    liftIO $ zipWithManyM_ (\out -> async . atomically . send out) updateOutputs botUpdates

    -- get the responses back
    updateWorldWithResponses

worldMain IncSPS = modifySPS (+10)
worldMain DecSPS = modifySPS (subtract 10)
worldMain ResetSPS = do
  sps <- asks (view ruleDefaultSPS)
  setSPS sps
worldMain Quit = liftIO exitSuccess

-- | Initialise the game.
worldInit :: (StW m, Ru m, MIO m, Ra m) => [BotSpec'] -> m ()
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

  -- initialise the input/outputs
  let doSpawn = unzipMany <$> replicateManyM numBots (liftIO . spawn $ bounded 1)
  (updateOutputs, updateInputs) <- doSpawn
  (responseOutput, responseInput) <- liftIO (spawn unbounded)

  -- start the bot threads, each with their own update channel
  rules <- ask
  let runBot' args =
        -- explicit 'case' is required due to existential quantification
        case args of
          (BotSpec' spec, botState, updIn) ->
            runReaderT (runBot spec botState updIn responseOutput) rules

  tids <- liftIO . mapManyM (forkIO . runBot')
                 $ zipMany3 (manyFromList specs)
                            bots
                            updateInputs

  -- store the channels in the world state
  wldBotOutputs .= updateOutputs
  wldBotInput .= Just responseInput

  -- get the responses to initialisation
  updateWorldWithResponses
  -- assign bots their thread IDs
  wldBots %= zipManyWith (set botTID) (fmap Just tids)

-- | Map character input to game events.
charToEvent :: Char -> Maybe Event
charToEvent '+' = Just IncSPS
charToEvent '-' = Just DecSPS
charToEvent '=' = Just ResetSPS
charToEvent 'q' = Just Quit
charToEvent _   = Nothing

-- | Encapsulates timing information to simplify 'stepController'.
data Timing =
  Timing
  { time0 :: Int
  , sps :: Int
  , stepsDone :: Int
  , sinceTick :: Int
  , timeNow :: Int
  }

-- | The main timing loop. Sends out events to the main world pipe whenever it
-- needs to do a game step.
stepController :: Rules -> MVar Int -> Producer Event IO ()
stepController rules spsVar =
  do let loop t@Timing{..} = do
           -- delay the thread so we don't put too much work on the processor
           lift (threadDelay 100)

           -- calculate the number of steps we *should* have got to by the current time
           let targetSteps = (timeNow - time0) * sps `div` 1000

           (stepsDone', sinceTick') <-
             -- if we need to do some steps...
             if stepsDone < targetSteps
             then
               do let tickSteps = rules^.ruleTickSteps
                      defaultSPS = rules^.ruleDefaultSPS
                      -- the effective amount of time that has passed should be
                      -- based on the default SPS in order to actually achieve the
                      -- speedup/slowdown
                      passed = 1 / fromIntegral defaultSPS

                  -- do we need to do a bot tick?
                  -- regardless, update steps done
                  if sinceTick >= tickSteps
                    then do yield (Step passed True)
                            return (stepsDone + 1, 0)
                    else do yield (Step passed False)
                            return (stepsDone + 1, sinceTick + 1)
             else return (stepsDone, sinceTick)

           -- see if we need to change the SPS
           newSPS <- lift $ tryTakeMVar spsVar
           nextTime <- lift getTime

           case newSPS of
             Just sps' ->
               -- if we do need to change the SPS, reset time0, steps done, etc
               loop
                 Timing
                 { time0 = nextTime
                 , sps = sps'
                 , stepsDone = 0
                 , sinceTick = 0
                 , timeNow = nextTime }
             Nothing ->
               -- otherwise, just update everything
               loop $ t { stepsDone = stepsDone'
                        , sinceTick = sinceTick'
                        , timeNow = nextTime }

     -- get the initial SPS from the variable
     sps <- lift $ takeMVar spsVar

     -- start the main timing loop
     time00 <- lift getTime
     loop
       Timing
       { time0 = time00
       , sps = sps
       , stepsDone = 0
       , sinceTick = 0
       , timeNow = time00
       }

-- | Run a battle with the given rules and robots.
--
-- If you have 'BotSpec's @mybot1@, @mybot2@ and @mybot3@, then
-- you can start a RoboMonad simulation from your main function
-- like so:
--
-- > main = runWorld defaultRules [mybot1, mybot2, mybot3]
runWorld :: Rules -> [BotSpec'] -> IO ()
runWorld rules specs = do
  -- work out the world dimensions
  let screenSize = rules^.ruleArenaSize
      (width, height) = (round (screenSize^._x), round (screenSize^._y)) :: (Int, Int)
      winConf = WindowConf width height "RoboMonad"

  -- initialise the world state
  let _wldBots       = mempty
      _wldRect       = Rect (screenSize ^* 0.5) screenSize 0
      _wldBullets    = mempty
      _wldSPS        = rules^.ruleDefaultSPS
      _wldBotOutputs = mempty
      _wldBotInput   = Nothing

  -- make the MVar that holds the steps per second
  _wldSPSVar <- newMVar _wldSPS

  -- use concurrent pipes to bring together input, animation and rendering
  (worldOut, worldIn) <- liftIO $ spawn (latest WorldState{..})
  (eventsOut, eventsIn) <- liftIO $ spawn unbounded

  let mainPipe = for cat $ \e ->
        do lift $ worldMain e
           yield =<< get

  let animationThread = do
        worldInit specs
        runEffect (fromInput eventsIn >-> mainPipe >-> toOutput worldOut)

      drawCheckQuit = do
        s <- await
        shouldQuit <- lift windowShouldClose
        when shouldQuit (void . liftIO . atomically $ send eventsOut Quit)
        return s

      renderingThread = do
        collectCharEvents eventsOut charToEvent
        runEffect (fromInput worldIn >-> (drawCheckQuit >~ renderWorld rules))

  _ <- async $ runEffect (stepController rules _wldSPSVar >-> toOutput eventsOut)
  _ <- async $ runRendering winConf renderingThread
  runReaderT (evalStateT animationThread WorldState{..}) rules
