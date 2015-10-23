{-|
Module      : Game.Robo.Render
Description : Provides a rendering framework that hides the underlying library used.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Robo.Render
  ( RenderData, renderDataWin
  , Colour
  , Draw
  , MonadDraw
  , startRender
  , getTicks
  , colour, colourWord
  , runDraw, drawPoly, drawLine, drawCircle
  , drawRender
  ) where

import           Control.Monad.Free.Church
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Data.Bits
import           Data.IORef
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import           Graphics.Rendering.OpenGL   as GL hiding (Line)
import           Graphics.UI.GLFW            as GL

import           Game.Robo.Core.Types.Maths

data RenderData = RenderData Window (IORef (Draw ()))

renderDataWin :: RenderData -> Window
renderDataWin (RenderData win _) = win

-------------------------------------
--  DRAWING
-------------------------------------

-- | A colour.
type Colour = Color3 GLfloat

-- | Something that can be drawn.
data DrawObject
  = Poly Colour (Vector Vec)
  | Line Colour Vec Vec
  | Circle Colour Vec Scalar
  deriving (Show)

-- | The functor for the free monad.
data DrawF a = DrawF DrawObject a
             deriving (Functor)

-- | The monad that you draw in.
newtype Draw a = Draw (F DrawF a)
               deriving (Functor, Applicative, Monad)

class Monad m => MonadDraw m where
  drawObject :: DrawObject -> m ()

instance MonadDraw m => MonadDraw (StateT s m) where
  drawObject = lift . drawObject

instance MonadDraw m => MonadDraw (ReaderT r m) where
  drawObject = lift . drawObject

instance (MonadDraw m, Monoid w) => MonadDraw (WriterT w m) where
  drawObject = lift . drawObject

instance (MonadDraw m, RandomGen g) => MonadDraw (RandT g m) where
  drawObject = lift . drawObject

instance MonadDraw Draw where
  drawObject obj = (Draw . liftF) (DrawF obj ())

-- | Make a colour from red, green and blue values.
colour :: Int -- ^ Red
       -> Int -- ^ Green
       -> Int -- ^ Blue
       -> Colour
colour r g b = Color3
                 (fromIntegral r / 255)
                 (fromIntegral g / 255)
                 (fromIntegral b / 255)

-- | Make a colour from a (probably hex) word.
colourWord :: Int -> Colour
colourWord w =
  let r = (w `shiftR` 16) .&. 0xFF
      g = (w `shiftR` 8 ) .&. 0xFF
      b = w .&. 0xFF
  in colour r g b

-- | Take a Draw monad and actually render the results to the screen,
-- filling the background with the given colour. Throws away the result.
runDraw :: Draw a -> RenderData -> IO ()
runDraw draw (RenderData _ drawVar) =
  writeIORef drawVar (void draw)

-- | Draw a polygon with the given colour through the given points.
drawPoly :: MonadDraw m => Colour -> Vector Vec -> m ()
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

-- | Convert a 2D vector to an OpenGL 3D vertex.
vecToVert :: Vec -> GL.Vertex3 GLfloat
vecToVert v =
  let (Vec x y) = (v / Vec 400 400) - Vec 1 1
  in GL.Vertex3 (realToFrac x) (realToFrac y) 0

-- | Do the actual drawing of a circle (approximated by a 20-sided polygon).
doCircle :: Colour -> Vec -> Scalar -> IO ()
doCircle col cen rad = GL.renderPrimitive GL.LineLoop $ do
  let atAng ang = GL.vertex . vecToVert $ cen + Vec (rad * cos ang) (rad * sin ang)
      numSegments = 20
  color col
  mapM_ (atAng . (*(2*pi)) . (/numSegments)) [0 .. numSegments - 1]

-- | Draw an object to the screen using GLUT.
doDrawObject :: DrawObject -> IO ()
doDrawObject obj =
  case obj of
    Poly col corners ->
      when (V.length corners > 1) $ do
        color col
        let onePoint = GL.vertex . vecToVert
        GL.renderPrimitive GL.LineLoop (V.mapM_ onePoint corners)
    Line col v1 v2 -> do
      color col
      GL.renderPrimitive GL.Lines $ do
        GL.vertex (vecToVert v1)
        GL.vertex (vecToVert v2)
    Circle col cen rad -> doCircle col cen rad

interpretDraw :: DrawF (IO a) -> IO a
interpretDraw (DrawF obj m) = do doDrawObject obj; m

doDraw :: Draw () -> IO ()
-- doDraw (Draw draw) = iterM interpretDraw draw
doDraw _ =
  do let randomVec =
           do x <- getRandomR (0 :: Double, 1)
              y <- getRandomR (0, 1)
              return (Vec x y)
     color (colour 255 0 0)
     GL.renderPrimitive GL.LineLoop $ replicateM_ 1000 (GL.vertex . vecToVert =<< randomVec)

-- | The function to actually do the displaying
-- display :: IORef (Vector3 GLfloat, GLfloat, GLfloat) -> RenderData -> IO ()
drawRender :: RenderData -> IO ()
drawRender (RenderData win drawVar) = do
  clearColor $= Color4 0.1 0.05 0.05 (1 :: GLclampf)
  clear [ ColorBuffer ]
  loadIdentity
  -- (offset, sx, sy) <- readIORef scaleVar
  -- scale sx sy 0
  -- translate offset
  -- translate (Vector3 (-1 :: GLfloat) (-1) 0)
  -- scale 0.01 0.01 (0 :: GLfloat)
  doDraw =<< readIORef drawVar

  writeIORef drawVar (return ())
  GL.swapBuffers win
  flush

-- | The GLUT reshape callback function.
-- reshape :: Vec -> IORef (Vector3 GLfloat, GLfloat, GLfloat) -> ReshapeCallback
-- reshape (Vec tw th) scaleVar (Size width height) = do
--   let offset = Vector3 (-realToFrac tw / 2) (-realToFrac th / 2) (0 :: GLfloat)
--       (sx, sy) = (realToFrac $ 2 / tw, realToFrac $ 2 / th) :: (GLfloat, GLfloat)
--   writeIORef scaleVar (offset, sx, -sy)
--   viewport $= (Position 0 0, Size (round tw) (round th))

-------------------------------------
--  BASICS
-------------------------------------

-- | Make a window to draw in.
startRender :: String -> Int -> Int -> IO RenderData
startRender windowName width height = do
  _ <- GL.init
  (Just w) <- createWindow (fromIntegral width) (fromIntegral height) windowName Nothing Nothing
  makeContextCurrent (Just w)

  -- Hack for retina displays
  (szx, szy) <- getFramebufferSize w
  viewport $= (Position 0 0, Size (fromIntegral szx) (fromIntegral szy))

  -- Set up rendering
  drawVar <- newIORef (return ())
  let render = RenderData w drawVar
  return render

-- | Get the number of milliseconds that have passed since initialisation.
getTicks :: IO Int
getTicks = do
  Just time <- getTime
  return . round $ time * 1000
