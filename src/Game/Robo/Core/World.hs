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

module Game.Robo.Core.World (runWorld) where

import           Lens.Family2
import           Lens.Family2.State

import           Control.Concurrent

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Traversable       (traverse)

import           Control.DeepSeq

import           Data.Array.IO
import           Data.Array.MArray

import           Data.Either
import           Data.List
import           Data.Maybe

import           Game.Robo.Core
import           Game.Robo.Core.Bot
import           Game.Robo.Render
import           Game.Robo.Render.World

-- | Move a bullet along.
updateBullet ∷ Double → Bullet → Bullet
updateBullet passed bul = bul & bulPos +~ (bul^.bulVel) |* passed

-- | Check if a bullet is within the bounds of the arena.
isBulletInArena ∷ Vec → Bullet → Bool
isBulletInArena size bul =
    bx >= minX && bx <= maxX && by >= minY && by <= maxY
  where
    (minX, minY, maxX, maxY) = (0, 0, size^.vX, size^.vY)
    (bx, by) = (bul^.bulPos.vX, bul^.bulPos.vY)

-- | Moves all the bullets along, and removes them if they are outside
-- the arena bounds.
stepBullets ∷ Double → World ()
stepBullets passed = do
  bullets <- use wldBullets
  size <- asks (view ruleArenaSize)
  let bullets' = filter (isBulletInArena size) . map (updateBullet passed) $ bullets
  wldBullets .= bullets'

-- | Splits a list of values into those satisfying a monadic predicate, and those not.
partitionM ∷ Monad m ⇒ (a → m Bool) → [a] → m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = do
  res <- f x
  (as, bs) <- partitionM f xs
  -- abuse of list comprehension!
  return ([x | res] ++ as, [x | not res] ++ bs)

-- | Handles all bullet collisions, removing colliding bullets and returning
-- collision data.
handleBulletCollisions ∷ World [BulletCollision]
handleBulletCollisions = do
    -- get the list of all bot IDs
    bids <- gets $ toListOf (wldBots.traverse.botID)
    bullets <- use wldBullets
    let bulletCheck bul = do
          -- find the first bot that the bullet is colliding with
          mhitId <- botsFindM (not <$> testBulletHit bul)
          -- if the bullet hit nothing, return it, otherwise return a collision
          return $ case mhitId of
                     Nothing -> Left bul
                     Just hitId -> Right $ BulletCollision (bul^.bulOwner) hitId (bul^.bulPower)

    (newBullets, collisions) <- partitionEithers <$> mapM bulletCheck bullets
    wldBullets .= newBullets
    return collisions

-- | Removes dead bots from the game, updates other bots' IDs.
pruneBots ∷ World [BotState]
pruneBots = do
  bots <- use wldBots
  let (alive, dead) = partition ((> 0) . view botLife) bots
  -- if there are dead bots, we need to get rid of them and assign new IDs to the rest
  if not (null dead) then do
    -- assign new bot IDs
    let updated = zipWith (set botID) [1..] alive
    wldBots .= updated

    return dead
  else return []

-- | Steps the world (minus the bots) forward a tick, returns a list of
-- bullet collisions.
stepWorld ∷ Double → World ([BulletCollision], [BotState])
stepWorld passed = do
  deadBots <- pruneBots
  stepBullets passed
  collisions <- handleBulletCollisions
  return (collisions, deadBots)

-- | Gets random positions within the given size for bots to start in.
generateSpawnPositions ∷ MonadRandom m ⇒ Int → Scalar → Vec → m [Vec]
generateSpawnPositions count margin size =
  replicateM count (getRandomR (1 |* margin, size - 1 |* (2*margin)))

-- | Collect the responses to a request from all the robots,
-- blocking until every robot has responded.
collectResponses ∷ Chan BotResponse → Int → IO [BotResponse]
collectResponses chan numBots = do
    responses <- newArray (1, numBots) Nothing
      :: IO (IOArray BotID (Maybe BotResponse))
    collect numBots responses
    results <- getElems responses
    return $ catMaybes results
  where
    collect 0 _ = return ()
    collect n responses = do
      response <- readChan chan
      writeArray responses (responseID response) (Just response)
      collect (n - 1) responses

-- | Collect responses and use them to update the world state.
updateWorldWithResponses ∷ Chan BotResponse → Int → IOWorld ()
updateWorldWithResponses chan numBots = do
  responses <- liftIO $ collectResponses chan numBots
  let newBots = map responseState responses
      bullets = concatMap responseBullets responses
  wldBots    .= newBots
  wldBullets %= (bullets ++)

-- | Gets the current time in milliseconds.
getTime ∷ IO Int
getTime = fromIntegral <$> getTicks

-- | Change the SPS, making sure to clamp it to between the min and max.
setSPS ∷ Int → World ()
setSPS newSPS = do
  minSPS <- asks (view ruleMinSPS)
  maxSPS <- asks (view ruleMaxSPS)
  let sps = case () of
              _ | newSPS < minSPS -> minSPS
                | newSPS > maxSPS -> maxSPS
                | otherwise       -> newSPS

  time <- use wldTime
  wldTime0     .= time
  wldStepsDone .= 0
  wldSPS       .= sps

-- | Modify the SPS with a pure function, clamping between the min and max.
modifySPS ∷ (Int → Int) → World ()
modifySPS f = do
  sps <- use wldSPS
  setSPS (f sps)

-- | The World's main logic action.
worldMain ∷ IOWorld ()
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
    (bulletCollisions, deadBots) <- promoteContext (stepWorld passed)
    let wasBotInvolved bid col = col^.bcolAggressor == bid
                              || col^.bcolVictim    == bid

    -- kill the threads running dead bots
    let maybeDo action marg = case marg of
          Just arg -> action arg
          Nothing -> return ()
    liftIO $ mapM_ (maybeDo killThread . view botTID) deadBots

    -- tell the bots to update themselves
    worldState <- get
    botStates  <- use wldBots
    let mkUpdate botState = BotUpdate
          { updateState  = botState
          , updateWorld  = worldState
          , updatePassed = passed
          , updateDoTick = doTick
          , updateBulletCollisions
            = filter (wasBotInvolved (botState^.botID)) bulletCollisions
          }
        botUpdates = map mkUpdate botStates

    -- get the channels out of the world state
    updateChans <- use wldUpdateChans
    Just responseChan <- use wldResponseChan
    liftIO $ zipWithM_ writeChan updateChans botUpdates

    -- get the responses back
    numBots <- length <$> use wldBots
    updateWorldWithResponses responseChan numBots

-- | Handle keyboard input.
worldKbd ∷ Char → IOWorld ()
worldKbd '+' = promoteContext $ modifySPS (+10)
worldKbd '-' = promoteContext $ modifySPS (subtract 10)
worldKbd '=' = do
  sps <- asks (view ruleDefaultSPS)
  promoteContext $ setSPS sps
worldKbd _ = return ()

-- | Initialise the game.
worldInit ∷ [BotSpec] → IOWorld ()
worldInit specs = do
  -- initialise the bot states
  let numBots = length specs
  mass        <- asks (view ruleMass)
  life        <- asks (view ruleMaxLife)
  spawnMargin <- asks (view ruleSpawnMargin)
  arenaSize   <- asks (view ruleArenaSize)
  positions   <- generateSpawnPositions numBots spawnMargin arenaSize
  let bots = zipWith (initialBotState life mass) [1..] positions
  wldBots .= bots

  -- initialise the channels
  updateChans  <- liftIO $ replicateM numBots newChan
  responseChan <- liftIO   newChan

  -- start the bot threads, each with their own update channel
  rules <- ask
  let runBot' (spec, botState, updChan) =
        runBot rules spec botState updChan responseChan
  tids <- liftIO $ mapM (forkIO . runBot') (zip3 specs bots updateChans)

  -- make sure the world knows what time it is (Robo time!)
  time <- liftIO getTime
  wldTime  .= time
  wldTime0 .= time

  -- get the responses to initialisation
  updateWorldWithResponses responseChan numBots
  -- assign bots their thread IDs
  wldBots %= zipWith (set botTID) (map Just tids)

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
runWorld ∷ Rules → [BotSpec] → IO ()
runWorld rules specs = do
  -- work out the world dimensions
  let screenSize = rules^.ruleArenaSize
      (width, height) = (round (screenSize^.vX), round (screenSize^.vY))

  -- initialise the world state
  let worldState = WorldState
        { _wldBots      = []
        , _wldRect      = Rect (screenSize |* 0.5) screenSize 0
        , _wldBullets   = []
        , _wldTime0     = 0
        , _wldTime      = 0
        , _wldStepsDone = 0
        , _wldSPS       = rules^.ruleDefaultSPS
        , _wldSinceTick = 0
        , _wldUpdateChans = []
        , _wldResponseChan = Nothing
        }

      worldActions = GameActions
        { actionInit = Just (worldInit specs)
        , actionMain = Just worldMain
        , actionDraw = Just drawWorld
        , actionKeyboard = Just worldKbd
        }

  -- start the game
  startGameLoop "RoboMonad" width height rules worldState worldActions
