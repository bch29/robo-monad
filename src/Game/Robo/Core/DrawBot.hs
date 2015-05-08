module Game.Robo.DrawBot where

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

import Game.Robo.Core
import Game.Robo.Maths

vecToPix :: Integral a => Vec -> (a, a)
vecToPix (Vector2 x y) = (round x, round y)

drawRect :: Surface -> Pixel -> Rect -> IO ()
drawRect surface pix box = do
  let corners = map vecToPix $ rectCorners box
      ln (a, b) (c, d) = void $ aaLine surface a b c d pix
  case corners of
    [a, b, c, d] -> do
      ln a b
      ln b c
      ln c d
      ln d a

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

drawChassis :: Surface -> BotState -> DrawWorld
drawChassis surface bot = do
  rules <- ask
  let box = botRect bot rules
  liftIO $ drawRect surface (Pixel 0x000000FF) box

drawGun :: Surface -> BotState -> DrawWorld
drawGun surface bot = do
  rules <- ask
  let box = gunRect bot rules
  liftIO $ drawRect surface (Pixel 0xFF0000FF) box

drawBot :: Surface -> BotState -> DrawWorld
drawBot surface bot = do
  let pos = bot^.botPos
  drawChassis surface bot
  drawGun     surface bot
