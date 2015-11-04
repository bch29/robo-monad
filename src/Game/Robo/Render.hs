{-|
Module      : Game.Robo.Render
Description : Provides a rendering framework that hides the underlying library used.
Copyright   : (c) Bradley Hardy, 2015
License     : GPL3
Maintainer  : bradleyhardy@live.com
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE Arrows              #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE NoMonomorphismRestriction        #-}
{-# LANGUAGE RankNTypes        #-}

module Game.Robo.Render where

import           Control.Arrow
import           Control.Concurrent (forkIO, ThreadId)
import           Control.Monad (forever)
import           Control.Monad.IO.Class
import           Data.IORef
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW
import           Graphics.UI.GLFW (WindowHint(..), getTime)
import           Lens.Micro.Platform

data ScalableRenderData = ScalableRenderData
  { srdPos  :: V2 Float
  , srdRot  :: V2 Float
  , srdSize :: Float
  , srdVert :: V4 Float
  , srdCol  :: V3 Float
  }

data ScalableBufferData = ScalableBufferData
  { sbdPos  :: B2 Float
  , sbdRot  :: B2 Float
  , sbdSize :: B Float
  , sbdVert :: B4 Float
  , sbdCol  :: B3 Float
  }

data ScalableShaderData = ScalableShaderData
  { ssdPos  :: V2 (S V Float)
  , ssdRot  :: V2 (S V Float)
  , ssdSize :: S V Float
  , ssdVert :: V4 (S V Float)
  , ssdCol  :: V3 (S V Float)
  }

instance BufferFormat ScalableBufferData where
  type HostFormat ScalableBufferData = ScalableRenderData
  toBuffer = proc ~(ScalableRenderData pos rot size vert col) -> do
    pos'  <- toBuffer -< pos
    rot'  <- toBuffer -< rot
    size' <- toBuffer -< size
    vert' <- toBuffer -< vert
    col'  <- toBuffer -< col
    returnA -< ScalableBufferData pos' rot' size' vert' col'

instance VertexInput ScalableBufferData where
  type VertexFormat ScalableBufferData = ScalableShaderData
  toVertex = proc ~(ScalableBufferData pos rot size vert col) -> do
    pos'  <- toVertex -< pos
    rot'  <- toVertex -< rot
    size' <- toVertex -< size
    vert' <- toVertex -< vert
    col'  <- toVertex -< col
    returnA -< ScalableShaderData pos' rot' size' vert' col'

-- | Multiply two vectors, treating them as imaginary numbers. Allows us to
-- rotate by an angle if we know the sine and cosine, which is faster in a
-- shader than recomputing the sine and cosine every time.
imaginaryMul :: Num a => V2 a -> V2 a -> V2 a
imaginaryMul (V2 a b) (V2 x y) = V2 (a * x - b * y) (a * y + b * x)
{-# INLINE imaginaryMul #-}

makeScaledCopies
  :: PrimitiveTopology p
  -> Buffer os (B4 Float)
  -> Buffer os (B2 Float, B2 Float, B Float, B3 Float)
  -> Render os f (PrimitiveArray p ScalableBufferData)
makeScaledCopies geom shape copies = do
  shapeArray <- newVertexArray shape
  copyArray <- newVertexArray copies
  return $
    toPrimitiveArrayInstanced
      geom
      (\vert (pos, rot, scale, col) -> ScalableBufferData pos rot scale vert col)
      shapeArray
      copyArray

scalableShader
  :: (ContextColorFormat c, Color c Bool ~ V3 Bool,
      Color c (S F (ColorElement c)) ~ V3 FFloat)
     => V2 Int
     -> PrimitiveStream p ScalableShaderData
     -> Shader os (ContextFormat c ds) b ()
scalableShader winSize stream = do
  let V2 winWidth winHeight = fmap fromIntegral winSize
      translateToWin (V2 x y) = V2 (2 * x / winWidth - 1)
                                   (1 - 2 * y / winHeight)
      compute (ScalableShaderData pos rot size vert col) =
        (over _xy (\v -> translateToWin (pos + size *^ imaginaryMul rot v)) vert, col)
      outputStream = fmap compute stream
  fragmentStream <- rasterize (const ( FrontAndBack
                                     , ViewPort (V2 0 0) winSize
                                     , DepthRange 0 1))
                              outputStream

  drawContextColor (const (ContextColorOption NoBlending (V3 True True True)))
                   fragmentStream

getTicks :: (MonadIO m, Integral a) => m a
getTicks = maybe 0 (round . (* 1000)) <$> liftIO getTime

runRendering
  :: WindowConf
     -> (forall os. Shader os (ContextFormat RGBFloat ()) shaderInput ())
     -> (forall os. CompiledShader os (ContextFormat RGBFloat ()) shaderInput ->
         ContextT GLFWWindow os (ContextFormat RGBFloat ()) IO ())
     -> IO (ThreadId, IORef Bool)
runRendering windowConf shaderCode renderAction = do
  let context = newContext' [WindowHint'Resizable False] windowConf
  windowCloseRef <- newIORef False
  tid <- forkIO $ runContextT context (ContextFormatColor RGB8) $ do
       shader <- compileShader shaderCode
       forever $
         do renderAction shader
            swapContextBuffers
            shouldClose <- windowShouldClose
            liftIO $ writeIORef windowCloseRef shouldClose
  return (tid, windowCloseRef)
