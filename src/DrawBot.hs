module DrawBot where

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

import Types
import Maths

vecToPix :: Integral a => Vec -> (a, a)
vecToPix (Vector2 x y) = (round x, round y)

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
      corners = map vecToPix $ rectCorners box

  liftIO . void $ aaPolygon surface corners (Pixel 0x000000FF)

drawGun :: Surface -> BotState -> DrawWorld
drawGun surface bot = do
  rules <- ask
  let box = gunRect bot rules
      corners = map vecToPix $ rectCorners box

  liftIO . void $ aaPolygon surface corners (Pixel 0xFF0000FF)

drawBot :: Surface -> BotState -> DrawWorld
drawBot surface bot = do
  let pos = bot^.botPos
  drawChassis surface bot
  drawGun     surface bot
