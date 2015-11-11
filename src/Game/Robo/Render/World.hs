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

module Game.Robo.Render.World (renderWorld) where

import           Control.Monad.Reader
import           Graphics.GPipe
import           Lens.Micro.Platform
import           Pipes
import           Control.Monad.Exception (MonadAsyncException)

import           Game.Robo.Core
import           Game.Robo.Render
import           Game.Robo.Render.Bot

data RoboShaderInput = RoboShaderInput
  { rsiBullets       :: PrimitiveArray Lines ScalableBufferData
  , rsiRobotBodies   :: PrimitiveArray Lines ScalableBufferData
  , rsiRobotRadars   :: PrimitiveArray Lines ScalableBufferData
  , rsiRobotGuns     :: PrimitiveArray Lines ScalableBufferData
  , rsiRobotLifebars :: PrimitiveArray Lines ScalableBufferData
  }

bulletCol :: V3 Float
bulletCol = mkRGB 0xFF 0x88 0x88

bulletBuf :: MonadIO m => Rules -> ContextT w os f m (Buffer os (B4 Float))
bulletBuf _ = do
  let numEdges :: Num a => a
      numEdges = 10
      mkPt i =
        let ang = i * 2 * pi / numEdges
        in V4 (cos ang) (sin ang) 0 1
      pts = map mkPt [0 .. numEdges]
  buf <- newBuffer numEdges
  writeBuffer buf 0 pts
  return buf

bulletInfo :: Bullet -> PASC
bulletInfo = do
  power <- realToFrac <$> view bulPower
  pos <- fmap realToFrac <$> view bulPos
  let size = sqrt power * 2.5
  return (pos, V2 1 0, size, bulletCol)

worldShader
  :: Rules
  -> Shader os (ContextFormat RGBFloat ds) RoboShaderInput ()
worldShader rules =
  do scalableShader screenSize =<< toPrimitiveStream rsiBullets
     scalableShader screenSize =<< toPrimitiveStream rsiRobotBodies
     scalableShader screenSize =<< toPrimitiveStream rsiRobotRadars
     scalableShader screenSize =<< toPrimitiveStream rsiRobotGuns
     scalableShader screenSize =<< toPrimitiveStream rsiRobotLifebars
  where screenSize = round <$> rules^.ruleArenaSize

type FiveX a = (a, a, a, a, a)

initRenderWorld
  :: MonadIO m
     => Rules
     -> ContextT w os f m (FiveX (Buffer os (B4 Float)))
initRenderWorld rules =
  (,,,,) <$> bulletBuf  rules
         <*> chassisBuf rules
         <*> gunBuf     rules
         <*> radarBuf   rules
         <*> lifebarBuf rules

makeInfoBuffers
  :: MonadIO m
     => Rules
     -> WorldState
     -> ContextT w os f m (FiveX (Buffer os (B2 Float, B2 Float, B2 Float, B3 Float)))
makeInfoBuffers rules worldState = do
  let bullets = worldState^.wldBullets
      bots = worldState^.wldBots

      mkBuffer f xs =
        do let len = length xs
           buf <- newBuffer len
           when (len > 0) $
             writeBuffer buf 0 (manyToList (fmap f xs))
           return buf

  (,,,,) <$> mkBuffer bulletInfo bullets
         <*> mkBuffer chassisInfo bots
         <*> mkBuffer gunInfo bots
         <*> mkBuffer radarInfo bots
         <*> mkBuffer (lifebarInfo rules) bots

performRendering
  :: (MonadIO m, MonadAsyncException m)
     => Rules
     -> FiveX (Buffer os (B4 Float))
     -> CompiledShader os (ContextFormat RGBFloat ds) RoboShaderInput
     -> Consumer WorldState (ContextT w os (ContextFormat RGBFloat ds) m) ()
performRendering rules (bullet, chassis, gun, radar, lifebar) shader = go
  where
    go = do lift . doRender =<< await; go
    doRender worldState =
      do (bulPosBuf, chassisPosBuf, gunPosBuf, radarPosBuf, lifebarPosBuf) <-
           makeInfoBuffers rules worldState

         render $ do
           clearContextColor (V3 0 0 0)
           rsiBullets       <- makeScaledCopies LineLoop bullet  bulPosBuf
           rsiRobotBodies   <- makeScaledCopies LineLoop chassis chassisPosBuf
           rsiRobotGuns     <- makeScaledCopies LineLoop gun     gunPosBuf
           rsiRobotRadars   <- makeScaledCopies LineLoop radar   radarPosBuf
           rsiRobotLifebars <- makeScaledCopies LineLoop lifebar lifebarPosBuf
           shader RoboShaderInput{..}

         swapContextBuffers

renderWorld
  :: (MonadIO m, MonadAsyncException m)
    => Rules
    -> Consumer WorldState (ContextT w os (ContextFormat RGBFloat ds) m) ()
renderWorld rules = do
  (buffers, shader) <-
    lift $ (,) <$> initRenderWorld rules
               <*> compileShader (worldShader rules)
  performRendering rules buffers shader
