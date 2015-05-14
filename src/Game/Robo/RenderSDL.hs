{-|
Module      : Game.Robo.Render
Description : Provides a rendering framework that hides the underlying library used.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

module Game.Robo.Render where
  -- ( RenderData
  -- , Colour
  -- , Draw
  -- , DrawObject
  -- , MonadDraw (..)
  -- , initRender
  -- , quitRender
  -- , withInitRender
  -- , makeRenderWindow
  -- , colour, colourWord
  -- , runDraw, drawPoly, drawLine, drawCircle
  -- ) where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives

import Control.Applicative
import Control.Monad.State

import Data.Bits

import Game.Robo.Core.Types.Maths

data RenderData = RenderData Surface

-------------------------------------
--  DRAWING
-------------------------------------

-- | A colour.
type Colour = Pixel

-- | Something that can be drawn.
data DrawObject = Poly Colour [Vec]
                | Line Colour Vec Vec
                | Circle Colour Vec Scalar

-- | The monad that you draw in.
newtype Draw a = Draw { unwrapDraw :: State [DrawObject] a }

instance Functor Draw where
  fmap f = Draw . fmap f . unwrapDraw

instance Applicative Draw where
  pure = Draw . return
  f <*> x = Draw (unwrapDraw f <*> unwrapDraw x)

instance Monad Draw where
  return = pure
  x >>= f = Draw (unwrapDraw x >>= (unwrapDraw . f))

class Monad m => MonadDraw m where
  drawObject :: DrawObject -> m ()

instance MonadDraw Draw where
  drawObject = Draw . modify . (:)

-- | Make a colour from red, green and blue values.
colour :: Int -- ^ Red
       -> Int -- ^ Green
       -> Int -- ^ Blue
       -> Colour
colour r g b = Pixel $ 0xFF
             + (fromIntegral r `shiftL` 24)
             + (fromIntegral g `shiftL` 16)
             + (fromIntegral b `shiftL` 8)

-- | Make a colour from a (probably hex) word.
colourWord :: Int -> Colour
colourWord = Pixel . (`shiftL` 8) . fromIntegral

-- | Take a Draw monad and actually render the results to the screen,
-- filling the background with the given colour, and returning the value
-- in the Draw monad.
runDraw :: Draw a -> Colour -> RenderData -> IO a
runDraw draw background render = do
  doDrawBackground render background
  let draws = unwrapDraw draw
      -- we reverse the list of objects so that the first ones put in
      -- the list are the first ones to be drawn
      (res, objects) = runState draws []
  mapM_ (doDrawObject render) (reverse objects)
  doFlip render
  return res

-- | Draw a polygon with the given colour through the given points.
drawPoly :: MonadDraw m => Colour -> [Vec] -> m ()
drawPoly col corners = drawObject (Poly col corners)

-- | Draw a line with the given colour through the given points.
drawLine :: MonadDraw m => Colour -> Vec -> Vec -> m ()
drawLine col a b = drawObject (Line col a b)

-- | Draw a circle with the given colour with the given centre and radius.
drawCircle :: MonadDraw m => Colour -> Vec -> Scalar -> m ()
drawCircle col cen rad = drawObject (Circle col cen rad)

-------------------------------------
--  INTERNALS
-------------------------------------

-- | Take a vector and return a pair of integers.
vecToPix :: Integral a => Vec -> (a, a)
vecToPix (Vec x y) = (round x, round y)

-- | Draw an object to the screen using SDL.
doDrawObject :: RenderData -> DrawObject -> IO ()
doDrawObject (RenderData surface) obj =
  let ln col a b = case (vecToPix a, vecToPix b) of
                     ((x1, y1), (x2, y2)) -> void $ aaLine surface x1 y1 x2 y2 col
  in  case obj of
        Poly col corners@(x:_) -> loop corners
          where loop [a] = ln col a x
                loop (a:b:rest) = ln col a b >> loop (b:rest)
                loop [] = return ()
        Poly _ _ -> return ()

        Line col v1 v2 -> ln col v1 v2

        Circle col cen rad ->
          case (vecToPix cen, round rad) of
            ((x, y), r) -> void $ aaCircle surface x y r col

-- | Draw the background using SDL.
doDrawBackground :: RenderData -> Colour -> IO ()
doDrawBackground (RenderData surface) (Pixel pix) = do
  -- We shift the pix right by 8 because fillRect doesn't take transparency into account.
  void $ fillRect surface Nothing (Pixel (pix `shiftR` 8))

-- | Flipping actually outputs the draw buffer to the screen.
doFlip :: RenderData -> IO ()
doFlip (RenderData surface) = SDL.flip surface

-------------------------------------
--  BASICS
-------------------------------------

-- | Initialise the rendering framework
initRender :: IO ()
initRender = SDL.init [InitEverything]

-- | Quit the rendering framework.
quitRender :: IO ()
quitRender = SDL.quit

-- | Initialise the rendering framework, do an IO action, then quit.
withInitRender :: IO () -> IO ()
withInitRender action = do
  initRender
  action
  quitRender

-- | Make a window to draw in.
makeRenderWindow :: Int -> Int -> IO RenderData
makeRenderWindow width height = do
  screen <- setVideoMode width height 32 [SWSurface]
  return $ RenderData screen

-- | Does the user want to close the window?
shouldQuit :: IO Bool
shouldQuit = do
  ev <- pollEvent
  return $ ev == Quit

-- | Get the amount of time that has passed since initialisation in milliseconds.
getTicks :: IO Int
getTicks = fromIntegral <$> SDL.getTicks

-- | Delay for the given number of milliseconds.
delay :: Int -> IO ()
delay = SDL.delay . fromIntegral
