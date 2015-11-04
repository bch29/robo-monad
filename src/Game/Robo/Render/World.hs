{-|
Module      : Game.Robo.Render.World
Description : Handles the rendering of the simulation using SDL.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}

module Game.Robo.Render.World where

import           Control.Monad.Reader
import           Data.IORef
import           Graphics.GPipe
import           Lens.Micro.Platform

import           Game.Robo.Core
import           Game.Robo.Render
import           Game.Robo.Render.Bot

data RoboShaderInput = RoboShaderInput
  { rsiBullets     :: PrimitiveArray Lines ScalableBufferData
  , rsiRobotBodies :: PrimitiveArray Lines ScalableBufferData
  , rsiRobotRadars :: PrimitiveArray Lines ScalableBufferData
  , rsiRobotGuns   :: PrimitiveArray Lines ScalableBufferData
  }

bulletCol :: V3 Float
bulletCol = mkRGB 0xFF 0x88 0x88

bulletBuf :: MonadIO m => Rules -> ContextT w os f m (Buffer os (B4 Float))
bulletBuf _ = do
  let numEdges :: Num a => a
      numEdges = 50
      mkPt i =
        let ang = i * 2 * pi / numEdges
        in V4 (cos ang) (sin ang) 0 1
      pts = map mkPt [0 .. numEdges]
  buf <- newBuffer numEdges
  writeBuffer buf 0 pts
  return buf

bulletInfo :: Bullet -> (V2 Float, V2 Float, Float, V3 Float)
bulletInfo = do
  power <- realToFrac <$> view bulPower
  pos <- fmap realToFrac <$> view bulPos
  let size = sqrt power * 2.5
  return (pos, V2 1 0, size, bulletCol)

worldShader
  :: (ContextColorFormat c,
      Color c Bool ~ V3 Bool,
      Color c (S F (ColorElement c)) ~ V3 FFloat) =>
     Rules ->  Shader os (ContextFormat c ds) RoboShaderInput ()
worldShader rules =
  do scalableShader screenSize =<< toPrimitiveStream rsiBullets
     scalableShader screenSize =<< toPrimitiveStream rsiRobotBodies
     scalableShader screenSize =<< toPrimitiveStream rsiRobotRadars
     scalableShader screenSize =<< toPrimitiveStream rsiRobotGuns
  where screenSize = round <$> rules^.ruleArenaSize

renderWorld
  :: (Num a, ContextColorFormat c,
      Color c Float ~ V3 a)
     => Rules
     -> IORef WorldState
     -> CompiledShader os (ContextFormat c ds) RoboShaderInput
     -- -> CompiledShader os (ContextFormat c ds) (PrimitiveArray Lines (B4 Float))
     -> ContextT w os (ContextFormat c ds) IO ()
renderWorld rules worldStateRef shader = do
  worldState <- liftIO (readIORef worldStateRef)

  bullet  <- bulletBuf  rules
  chassis <- chassisBuf rules
  gun     <- gunBuf     rules
  radar   <- radarBuf   rules

  let bullets = worldState^.wldBullets
      bots    = worldState^.wldBots

      bulletInfos  = fmap bulletInfo  bullets
      chassisInfos = fmap chassisInfo bots
      gunInfos     = fmap gunInfo     bots
      radarInfos   = fmap radarInfo   bots

      mkBuffer v = do buf <- newBuffer (length v)
                      writeBuffer buf 0 (manyToList v)
                      return buf

  bulPosBuf     <- mkBuffer bulletInfos
  chassisPosBuf <- mkBuffer chassisInfos
  gunPosBuf     <- mkBuffer gunInfos
  radarPosBuf   <- mkBuffer radarInfos

  render $ do
    clearContextColor (V3 0 0 0)
    rsiBullets     <- makeScaledCopies LineLoop bullet  bulPosBuf
    rsiRobotBodies <- makeScaledCopies LineLoop chassis chassisPosBuf
    rsiRobotGuns   <- makeScaledCopies LineLoop gun     gunPosBuf
    rsiRobotRadars <- makeScaledCopies LineLoop radar   radarPosBuf
    shader RoboShaderInput{..}

