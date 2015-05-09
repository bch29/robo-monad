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

import Data.Vector.V2
import Data.Vector.Class

import Game.Robo.Core.Types
import Game.Robo.Maths

vecToPix :: Integral a => Vec -> (a, a)
vecToPix (Vector2 x y) = (round x, round y)

-- drawPoly :: Surface -> Pixel -> [(Int16, Int16)] -> IO ()
drawPoly surface pix corners@(x:_) = loop corners
  where loop [a] = ln a x
        loop (a:b:rest) = ln a b >> loop (b:rest)
        ln (a, b) (c, d) = void $ aaLine surface a b c d pix

drawRect :: Surface -> Pixel -> Rect -> IO ()
drawRect surface pix box = do
  let corners = map vecToPix $ rectCorners box
  drawPoly surface pix corners

botRect :: BotState -> BattleRules -> Rect
botRect bot rules =
  let sz  = rules^.ruleBotSize
      ang = bot^.botHeading
      pos = bot^.botPos
  in  rect pos sz ang

gunRect :: BotState -> BattleRules -> Rect
gunRect bot rules =
  let sz  = rules^.ruleGunSize
      botAng = bot^.botHeading
      gunAng = (bot^.botGun.gunHeading) + (bot^.botHeading)
      dir = vecFromAngle gunAng
      offset = dir |* (sz^.vX) * 0.5
      pos = (bot^.botPos) + offset
  in  rect pos sz gunAng

radarRect :: BotState -> BattleRules -> Rect
radarRect bot rules =
  let sz  = rules^.ruleRadarSize
      botAng = bot^.botHeading
      radAng = (bot^.botRadar.radHeading) + (bot^.botHeading)
      dir = vecFromAngle radAng
      offset = dir |* (sz^.vX) * 0.5
      pos = (bot^.botPos) + offset
  in  rect pos sz radAng

drawChassis :: Surface -> BotState -> DrawWorld
drawChassis surface bot = do
  rules <- ask
  let box = botRect bot rules
  liftIO $ drawRect surface (Pixel 0x99FFFFFF) box

drawGun :: Surface -> BotState -> DrawWorld
drawGun surface bot = do
  rules <- ask
  let box = gunRect bot rules
  liftIO $ drawRect surface (Pixel 0xFFFFAAFF) box

drawRadar :: Surface -> BotState -> DrawWorld
drawRadar surface bot = do
  rules <- ask
  let box = radarRect bot rules
      pos = vecToPix (bot^.botPos)
      [a, b, c, d] = map vecToPix $ rectCorners box

  liftIO $ drawPoly surface (Pixel 0xFFFFFF88) [a, b, pos]

drawBot :: Surface -> BotState -> DrawWorld
drawBot surface bot = do
  let pos = bot^.botPos
  drawChassis surface bot
  drawGun     surface bot
  drawRadar   surface bot
