{-|
Module      : Game.Robo.Render.Bot
Description : Handles drawing of robots using SDL.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Game.Robo.Render.Bot where

import           Control.Monad.Reader
import           Lens.Micro.Platform

import           Game.Robo.Core
import           Game.Robo.Render
import           Graphics.GPipe

mkRGB :: Int -> Int -> Int -> V3 Float
mkRGB r g b = fmap ((/ 255.0) . fromIntegral) (V3 r g b)

rectBuf
  :: MonadIO m
     => V2 Float -- ^ Size
     -> V2 Float -- ^ Offset from centre
     -> ContextT w os f m (Buffer os (B4 Float))
rectBuf size offset = do
  let V2 x y = size ^/ 2
      pts = [ V2 (-x) (-y)
            , V2 (-x) y
            , V2 x y
            , V2 x (-y) ]
      f v = V4 0 0 0 1 & _xy .~ (v + offset)
  buf <- newBuffer 4
  writeBuffer buf 0 (map f pts)
  return buf

chassisBuf :: MonadIO m => Rules -> ContextT w os f m (Buffer os (B4 Float))
chassisBuf rules = do
  let size = realToFrac <$> view ruleBotSize rules
  rectBuf size 0

gunBuf :: MonadIO m => Rules -> ContextT w os f m (Buffer os (B4 Float))
gunBuf rules = do
  let size = realToFrac <$> view ruleGunSize rules
  let off = V2 (size^._x / 2) 0
  rectBuf size off

radarBuf :: MonadIO m => Rules -> ContextT w os f m (Buffer os (B4 Float))
radarBuf rules = do
  let size = realToFrac <$> view ruleRadarSize rules
  let V2 x y = size ^/ 2
      pts = [0, V2 (-x) (-y), V2 (-x) y]
      f v = V4 0 0 0 1 & _xy .~ v
  buf <- newBuffer 3
  writeBuffer buf 0 (map f pts)
  return buf

chassisColour, gunColour, radarColour :: V3 Float
chassisColour = mkRGB 0x99 0xFF 0xFF
gunColour = mkRGB 0xFF 0xFF 0xAA
radarColour = mkRGB 0xFF 0xFF 0x88

chassisInfo :: BotState -> (V2 Float, V2 Float, Float, V3 Float)
chassisInfo = do
  pos <- fmap realToFrac <$> view botPos
  ang <- realToFrac <$> view botHeading
  return (pos, V2 (cos ang) (sin ang), 1, chassisColour)

gunInfo :: BotState -> (V2 Float, V2 Float, Float, V3 Float)
gunInfo = do
  pos <- fmap realToFrac <$> view botPos
  botAng <- realToFrac <$> view botHeading
  gunAng <- realToFrac <$> view (botGun.gunHeading)
  let ang = botAng + gunAng
  return (pos, V2 (cos ang) (sin ang), 1, gunColour)

radarInfo :: BotState -> (V2 Float, V2 Float, Float, V3 Float)
radarInfo = do
  pos <- fmap realToFrac <$> view botPos
  botAng <- realToFrac <$> view botHeading
  radAng <- realToFrac <$> view (botRadar.radHeading)
  let ang = botAng + radAng
  return (pos, V2 (cos ang) (sin ang), 1, radarColour)

-- lifeBuf :: MonadIO m => V3 Float -> ContextT w os f m (Buffer os (B4 Float))
-- lifeBuf = do

-- drawLife :: RenderBot m => m ()
-- drawLife = do
--   life <- bs botLife

--   maxL <- ru ruleMaxLife
--   off <- ru ruleLifebarOffset
--   (Vec maxw h) <- ru ruleLifebarSize

--   pos <- (+) <$> bs botPos <*> pure off
--   let life' = if life < 0 then 0 else life
--       rp = (maxL - life') / maxL
--       gp = life / (maxL / 2)
--       box = Rect pos (Vec (maxw * life' / maxL) h) 0
--   drawRect (colour (round $ rp * 255) (round $ gp * 255) 0x00) box

