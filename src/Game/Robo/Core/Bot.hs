{-|
Module      : Game.Robo.Core.Bot
Description : Deals with robot threads and physics.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE PartialTypeSignatures    #-}

module Game.Robo.Core.Bot where

import           Lens.Micro.Platform

import           Control.Concurrent

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

import           Data.List hiding (any, null)
import           Data.Ord
import qualified Data.Vector as V
import           Data.Vector (Vector)

import           Game.Robo.Core
import           Game.Robo.Maths

type PureBot m = (MonadState BotState m, MonadReader Rules m)

initialBotState ::  Scalar -> Scalar -> BotID -> Vec -> BotState
initialBotState life mass bid pos = BotState
  { _botID        = bid
  , _botTID       = Nothing
  , _botThrust    = 0
  , _botAngThrust = 0
  , _botPos       = pos
  , _botHeading   = 0
  , _botSpeed     = 0
  , _botAngVel    = 0

  , _botGun   = GunState   { _gunHeading = 0
                           , _gunFiring  = 0
                           , _gunAngVel  = 0
                           , _gunAngAcc  = 0 }

  , _botRadar = RadarState { _radHeading = 0
                           , _radAngVel  = 0 }

  , _botMass      = mass
  , _botEnergy    = 0
  , _botLife      = life
  }

-- | Returns the 'Rect' surrounding the robot (with its axis aligned with the
-- robot's heading direction).
botRect :: MonadReader Rules m => BotState -> m Rect
botRect botState = do
  sz  <- asks (view ruleBotSize)
  let ang = botState^.botHeading
      pos = botState^.botPos
  return (Rect pos sz ang)

-- | Is the robot colliding with a wall, and if so which wall?
-- The wall is indicated by its direction (i.e. @Vec 1 0@ for the
-- right wall).
tryCollideWall :: PureBot m => Rect -> m (Maybe WallCollisionData)
tryCollideWall arenaRect = do
  rect <- botRect =<< get
  let crs = rectCornersOutside arenaRect rect
      -- get the width and height of the arena
      (Rect _ (Vec sx sy) _) = arenaRect
  case crs of
    [] -> return Nothing
    vec : _ -> do
      heading <- use botHeading
      let dir = case vec of
                  Vec x y | x > sx    -> Vec 1 0
                          | x < 0     -> Vec (-1) 0
                          | y > sy    -> Vec 0 1
                          | y < 0     -> Vec 0 (-1)
                          | otherwise -> error "tryCollideWall"
          angle = angNormRelative $ angleToHorizontal dir - heading + pi/2
      return . Just $ WallCollisionData angle

-- | Calculate the robot's new position after some time has passed.
-- Approximates many integrals. Returns the wall collision data if
-- a wall was hit.
stepBotMotion :: PureBot m => Double -> WorldState -> m (Maybe WallCollisionData)
stepBotMotion passed worldState = do
  driveFric <- asks (view ruleDriveFriction)
  turnFric  <- asks (view ruleTurnFriction)

  angThrust <- use botAngThrust
  angVel    <- use botAngVel
  heading   <- use botHeading

  let angVel'  = turnFric * (angVel + angThrust * passed)
      heading' = angNormAbsolute $ heading + angVel' * passed
      dir      = vecFromAngle heading'

  thrust    <- use botThrust
  mass      <- use botMass
  speed     <- use botSpeed
  pos       <- use botPos
  let acc    = thrust / mass
      speed' = driveFric * (speed + acc * passed)
      pos'   = pos + dir |* (speed' * passed)

  -- stop the robot in its tracks if it has hit a wall,
  -- otherwise update its position and direction
  botPos     .= pos'
  botHeading .= heading'
  hitWall <- tryCollideWall (worldState^.wldRect)
  case hitWall of
    Nothing -> do
      botAngVel .= angVel'
      botSpeed  .= speed'
    Just _  -> do
      botPos     .= pos
      botHeading .= heading
      botAngVel  .= 0
      botSpeed   .= 0

  return hitWall

-- | If it is possible to fire with the amount of energy we have,
-- create a bullet moving away from the end of the robot's gun,
-- subtract the energy cost, and set the firepower back to 0.
fireBullet :: PureBot m => m (Vector Bullet)
fireBullet = do
  energy <- use botEnergy
  firing <- use (botGun.gunFiring)
  if firing <= energy then do
    pos     <- use botPos
    bid     <- use botID
    speed   <- asks (view ruleBulletSpeed)
    gunSize <- asks (view ruleGunSize)
    botAng  <- use botHeading
    gunAng  <- use (botGun.gunHeading)
    let ang = botAng + gunAng
        vel    = rotateVec ang (Vec speed 0)
        offset = rotateVec ang (Vec (gunSize^.vX) 0)
        bul    = Bullet { _bulVel   = vel
                        , _bulPos   = pos + offset
                        , _bulPower = firing
                        , _bulOwner = bid }

    botGun.gunFiring .= 0
    botEnergy .= (energy - firing)
    return (V.singleton bul)
  else
    return mempty

-- | Update the robot's gun, returning a list of newly fired bullets.
stepBotGun :: PureBot m => Double -> Bool -> m (Vector Bullet)
stepBotGun passed doTick = do
  -- motion
  gunFric <- asks (view ruleGunFriction)
  acc <- use (botGun.gunAngAcc)
  vel <- use (botGun.gunAngVel)
  heading <- use (botGun.gunHeading)

  let vel' = gunFric * (vel + acc * passed)
      heading' = angNormAbsolute (heading + vel' * passed)

  botGun.gunAngVel  .= vel'
  botGun.gunHeading .= heading'

  -- firing
  firing <- use (botGun.gunFiring)
  if doTick && firing > 0
     then fireBullet
     else return mempty

-- | Deal with energy regeneration.
stepBotEnergy :: PureBot m => Double -> m ()
stepBotEnergy passed = do
  energy <- use botEnergy
  maxEnergy <- asks (view ruleMaxEnergy)
  perSecond <- asks (view ruleEnergyRechargeRate)
  let energy' = energy + perSecond * passed
  if energy' <= maxEnergy
     then botEnergy .= energy'
     else botEnergy .= maxEnergy

-- | Update the motion of the robot's radar.
stepBotRadar :: PureBot m => Double -> m ()
stepBotRadar passed = do
  vel <- use (botRadar.radAngVel)
  botRadar.radHeading += vel * passed

-- | Scans for other robots within this robot's field of view.
tryScan :: PureBot m => Vector BotState -> m (Maybe ScanData)
tryScan bots = do
  pos   <- use botPos
  bid   <- use botID
  fov   <- asks (view ruleRadFOV)
  range <- asks (view ruleRadRange)
  rh    <- use (botRadar.radHeading)
  bh    <- use botHeading
  let facingAngle = rh + bh
      minAngle = facingAngle - fov / 2
      maxAngle = facingAngle + fov / 2
      -- make sure the current bot isn't included
      isNotUs = (/= bid) . view botID
      -- get all bots within the scan segment
      isInScanSegment = inSegmentCentre minAngle maxAngle range pos . view botPos

      filterScannable = V.filter (\b -> isNotUs b && isInScanSegment b)
      -- sort by distance
      sortByDistance  = sortBy (comparing (vecMag . subtract pos . view botPos)) . V.toList

  -- return the closet bot, if it exists
  return $ case sortByDistance . filterScannable $ bots of
    bot : _ ->
      let thatPos = bot^.botPos
          dist = vecMag (pos - thatPos)
          ang  = angNormAbsolute $ pos `angleTo` thatPos
      in  Just $ ScanData dist ang
    _       -> Nothing

-- | Tests if a bullet has hit the robot, and returns True if so.
testBulletHit :: MonadReader Rules m => BotState -> Bullet -> m Bool
testBulletHit botState bul = do
  box <- botRect botState
  let bid = botState^.botID
      bpos = bul^.bulPos
      owner = bul^.bulOwner
  return $ owner /= bid && withinRect bpos box

-- | Print a log from a robot with the given name to the console.
writeLog :: String -> [String] -> IO ()
writeLog name = putStr . unlines . map ((name ++ ": ") ++)

botMain :: BotSpec -> Chan BotUpdate -> Chan BotResponse -> IOBot ()
botMain spec updateChan responseChan =
  case spec of
    BotSpec{..} -> do
      -- run the robot's initialisation method, listening to the log so that
      -- we can print it out
      ((_, userState1), lg) <- listen $ runRobo onInit botInitialState
      state' <- get

      liftIO $ putStr lg

      -- send the initialisation results back to the main thread
      initialBid <- use botID
      liftIO $ writeChan responseChan BotResponse
        { responseID = initialBid
        , responseState = state'
        , responseBullets = mempty }

      let step userState = do
            -- wait until the main thread tells us to advance
            BotUpdate{..} <- liftIO $ readChan updateChan

            -- sync up our state with what the world thinks our state is
            put updateState
            bid <- use botID

            let wasAggressor col = col^.bcolAggressor == bid
                wasVictim    col = col^.bcolVictim    == bid
                victimCollisions = V.filter wasVictim updateBulletCollisions
                bulletHit = any wasAggressor updateBulletCollisions
                wasHit    = not (V.null victimCollisions)
                damageReceived = V.sum . V.map (view bcolPower) $ victimCollisions

            when wasHit $ botLife -= damageReceived

            -- update the robot state
            (bullets, userState') <- do
              -- motion
              mwcol <- stepBotMotion updatePassed updateWorld
              -- gun rotation / firing
              bullets <- stepBotGun updatePassed updateDoTick
              -- energy
              stepBotEnergy updatePassed
              -- radar rotation
              stepBotRadar updatePassed
              -- radar scanning
              mscan <- tryScan (updateWorld^.wldBots)

              -- run the user callbacks
              let roboActions = do
                    -- nullary actions
                    when wasHit    onHitByBullet
                    when bulletHit onBulletHit
                    when updateDoTick    onTick
                    -- unary actions
                    maybe (return ()) onScan        mscan
                    maybe (return ()) onCollideWall mwcol
              (_, userState') <- runRobo roboActions userState
              return (bullets, userState')

            -- send our new state, and any bullets fired, back to the main thread
            botState' <- get
            liftIO $ writeChan responseChan BotResponse
              { responseID      = bid
              , responseState   = botState'
              , responseBullets = bullets }

            -- loop indefinitely (until the main thread dies)
            return $ Just userState'
      -- start the main loop
      iterateContext userState1 step

-- | Run a robot. This never terminates and is designed to be called in its own thread.
-- Communicates with the World thread via channels.
runBot :: Rules -> BotSpec -> BotState -> Chan BotUpdate -> Chan BotResponse -> IO ()
runBot rules spec botState updateChan responseChan =
  evalContext (botMain spec updateChan responseChan) rules botState
