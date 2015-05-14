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

import Lens.Family2
import Lens.Family2.State
import Lens.Family2.Stock

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.Traversable
import Control.Applicative ((<$>))

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
  let [a, b, c, d] = rectCorners box
  drawPoly (colour 0xFF 0xFF 0x88) [a, b, pos]

drawBot :: DrawBot ()
drawBot = do
  pos <- use botPos
  drawChassis
  drawGun
  drawRadar
