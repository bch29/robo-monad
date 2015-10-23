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

module Game.Robo.Render.World where

import           Control.Monad.Reader
import qualified Data.Vector          as V
import           Lens.Micro.Platform

import           Game.Robo.Core
import           Game.Robo.Render
import           Game.Robo.Render.Bot

ws :: MonadReader (b, WorldState) m => Lens' WorldState a -> m a
ws l = view (_2.l)

drawBullet :: Bullet -> DrawWorld ()
drawBullet bul = do
  let pos = bul^.bulPos
      pow = bul^.bulPower
      size = sqrt pow * 2.5

  drawCircle (colour 0xFF 0x88 0x88) pos size

drawWorld :: DrawWorld ()
drawWorld = do
  botStates <- ws wldBots
  (rules, _) <- ask
  mapM_ (\botState -> runReaderT drawBot (rules, botState)) botStates

  buls <- ws wldBullets
  mapM_ drawBullet buls

  Vec w h <- ru ruleArenaSize

  drawPoly (colourWord 0xFFFFFF) (V.fromList [Vec 0 0, Vec 0 h, Vec w h, Vec w 0])
