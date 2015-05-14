{-|
Module      : Game.Robo.Render.World
Description : Handles the rendering of the simulation using SDL.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

module Game.Robo.Render.World where

import Lens.Family2
import Lens.Family2.State

import Game.Robo.Core
import Game.Robo.Render
import Game.Robo.Render.Bot

drawBullet :: Bullet -> DrawWorld ()
drawBullet bul = do
  let pos = bul^.bulPos
      pow = bul^.bulPower
      size = sqrt pow * 2.5

  drawCircle (colour 0xFF 0x88 0x88) pos size

drawWorld :: DrawWorld ()
drawWorld = do
    -- liftIO . void $ fillRect surface Nothing (Pixel 0xFF100808)

    bots <- use wldBots
    let drawBot' bot = applyBot (bot^.botID) drawBot
    mapM_ drawBot' bots

    buls <- use wldBullets
    mapM_ drawBullet buls
