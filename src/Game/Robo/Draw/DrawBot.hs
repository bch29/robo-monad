{-|
Module      : Game.Robo.Draw.DrawBot
Description : Handles drawing of robots using SDL.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable (depends on SDL)

-}

module Game.Robo.Draw.DrawBot where

import Graphics.UI.SDL as SDL hiding (Rect)
import Graphics.UI.SDL.Primitives
import Graphics.UI.SDL.Rotozoomer as GFX

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

vecToPix :: Integral a => Vec -> (a, a)
vecToPix (Vec x y) = (round x, round y)

-- drawPoly :: Surface -> Pixel -> [(Int16, Int16)] -> IO ()
drawPoly surface pix corners@(x:_) = loop corners
  where loop [a] = ln a x
        loop (a:b:rest) = ln a b >> loop (b:rest)
        ln (a, b) (c, d) = void $ aaLine surface a b c d pix

drawRect :: Surface -> Pixel -> Rect -> IO ()
drawRect surface pix box = do
  let corners = map vecToPix $ rectCorners box
  drawPoly surface pix corners

botRect :: IOBot Rect
botRect = do
  sz <- asks (view ruleBotSize)
  ang <- use botHeading
  pos <- use botPos
  return $ Rect pos sz ang

gunRect :: IOBot Rect
gunRect = do
  sz <- asks (view ruleGunSize)
  botAng <- use botHeading
  gunAng <- (botAng +) <$> use (botGun.gunHeading)
  let dir = vecFromAngle gunAng
      offset = dir |* (sz^.vX) * 0.5
  pos <- (+ offset) <$> use botPos
  return $ Rect pos sz gunAng

radarRect :: IOBot Rect
radarRect = do
  sz <- asks (view ruleRadarSize)
  botAng <- use botHeading
  radAng <- (botAng +) <$> use (botRadar.radHeading)
  let dir = vecFromAngle radAng
      offset = dir |* (sz^.vX) * 0.5
  pos <- (+ offset) <$> use botPos
  return $ Rect pos sz radAng

drawChassis :: Surface -> IOBot ()
drawChassis surface = do
  box <- botRect
  liftIO $ drawRect surface (Pixel 0x99FFFFFF) box

drawGun :: Surface -> IOBot ()
drawGun surface = do
  box <- gunRect
  liftIO $ drawRect surface (Pixel 0xFFFFAAFF) box

drawRadar :: Surface -> IOBot ()
drawRadar surface = do
  box <- radarRect
  pos <- vecToPix <$> use botPos
  let [a, b, c, d] = map vecToPix $ rectCorners box

  liftIO $ drawPoly surface (Pixel 0xFFFFFF88) [a, b, pos]

drawBot :: Surface -> IOBot ()
drawBot surface = do
  pos <- use botPos
  drawChassis surface
  drawGun     surface
  drawRadar   surface
