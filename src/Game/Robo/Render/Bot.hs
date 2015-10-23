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
import qualified Data.Vector          as V
import           Lens.Micro.Platform

import           Game.Robo.Core
import           Game.Robo.Maths
import           Game.Robo.Render

type RenderBot m = (MonadReader (Rules, BotState) m, MonadDraw m)

ru :: MonadReader (Rules, b) m => Lens' Rules a -> m a
ru l = view (_1.l)

bs :: MonadReader (b, BotState) m => Lens' BotState a -> m a
bs l = view (_2.l)

drawRect :: RenderBot m => Colour -> Rect -> m ()
drawRect col box =
  drawPoly col (V.fromList $ rectCorners box)

botRect :: RenderBot m => m Rect
botRect = do
  sz <- ru ruleBotSize
  ang <- bs botHeading
  pos <- bs botPos
  return $ Rect pos sz ang

gunRect :: RenderBot m => m Rect
gunRect = do
  sz <- ru ruleGunSize
  botAng <- bs botHeading
  gunAng <- (botAng +) <$> bs (botGun.gunHeading)
  let dir = vecFromAngle gunAng
      offset = dir |* (sz^.vX) * 0.5
  pos <- (+ offset) <$> bs botPos
  return $ Rect pos sz gunAng

radarRect :: RenderBot m => m Rect
radarRect = do
  sz <- ru ruleRadarSize
  botAng <- bs botHeading
  radAng <- (botAng +) <$> bs (botRadar.radHeading)
  let dir = vecFromAngle radAng
      offset = dir |* (sz^.vX) * 0.5
  pos <- (+ offset) <$> bs botPos
  return $ Rect pos sz radAng

drawChassis :: RenderBot m => m ()
drawChassis = do
  box <- botRect
  drawRect (colour 0x99 0xFF 0xFF) box

drawGun :: RenderBot m => m ()
drawGun = do
  box <- gunRect
  drawRect (colour 0xFF 0xFF 0xAA) box

drawRadar :: RenderBot m => m ()
drawRadar = do
  box <- radarRect
  pos <- bs botPos
  let [a, b, _, _] = rectCorners box
  drawPoly (colour 0xFF 0xFF 0x88) (V.fromList [a, b, pos])

drawLife :: RenderBot m => m ()
drawLife = do
  life <- bs botLife

  maxL <- ru ruleMaxLife
  off <- ru ruleLifebarOffset
  (Vec maxw h) <- ru ruleLifebarSize

  pos <- (+) <$> bs botPos <*> pure off
  let life' = if life < 0 then 0 else life
      rp = (maxL - life') / maxL
      gp = life / (maxL / 2)
      box = Rect pos (Vec (maxw * life' / maxL) h) 0
  drawRect (colour (round $ rp * 255) (round $ gp * 255) 0x00) box

drawBot :: RenderBot m => m ()
drawBot = do
  drawChassis
  drawGun
  drawRadar
  drawLife
