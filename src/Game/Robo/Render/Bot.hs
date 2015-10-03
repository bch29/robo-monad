{-|
Module      : Game.Robo.Render.Bot
Description : Handles drawing of robots using SDL.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

module Game.Robo.Render.Bot where

import Lens.Micro.Platform

import Control.Monad.Reader

import Game.Robo.Core
import Game.Robo.Maths
import Game.Robo.Render

drawRect :: Colour -> Rect -> DrawBot ()
drawRect col box =
  drawPoly col (rectCorners box)

botRect :: DrawBot Rect
botRect = do
  sz <- asks (view ruleBotSize)
  ang <- use botHeading
  pos <- use botPos
  return $ Rect pos sz ang

gunRect :: DrawBot Rect
gunRect = do
  sz <- asks (view ruleGunSize)
  botAng <- use botHeading
  gunAng <- (botAng +) <$> use (botGun.gunHeading)
  let dir = vecFromAngle gunAng
      offset = dir |* (sz^.vX) * 0.5
  pos <- (+ offset) <$> use botPos
  return $ Rect pos sz gunAng

radarRect :: DrawBot Rect
radarRect = do
  sz <- asks (view ruleRadarSize)
  botAng <- use botHeading
  radAng <- (botAng +) <$> use (botRadar.radHeading)
  let dir = vecFromAngle radAng
      offset = dir |* (sz^.vX) * 0.5
  pos <- (+ offset) <$> use botPos
  return $ Rect pos sz radAng

drawChassis :: DrawBot ()
drawChassis = do
  box <- botRect
  drawRect (colour 0x99 0xFF 0xFF) box

drawGun :: DrawBot ()
drawGun = do
  box <- gunRect
  drawRect (colour 0xFF 0xFF 0xAA) box

drawRadar :: DrawBot ()
drawRadar = do
  box <- radarRect
  pos <- use botPos
  let [a, b, _, _] = rectCorners box
  drawPoly (colour 0xFF 0xFF 0x88) [a, b, pos]

drawLife :: DrawBot ()
drawLife = do
  life <- use botLife

  maxL <- asks (view ruleMaxLife)
  off <- asks (view ruleLifebarOffset)
  (Vec maxw h) <- asks (view ruleLifebarSize)

  pos <- (+ off) <$> use botPos
  let life' = if life < 0 then 0 else life
      rp = (maxL - life') / maxL
      gp = life / (maxL / 2)
      box = Rect pos (Vec (maxw * life' / maxL) h) 0
  drawRect (colour (round $ rp * 255) (round $ gp * 255) 0x00) box

drawBot :: DrawBot ()
drawBot = do
  drawChassis
  drawGun
  drawRadar
  drawLife
