{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
module Game.Robo.Core.Input where

import           Graphics.GPipe.Context
import           Graphics.GPipe.Context.GLFW.Unsafe
import           Graphics.UI.GLFW
import           Pipes
import           Pipes.Concurrent

-- | Collects char events from GLFW and feeds them to the given output after
-- mapping them to some value. Ignores any characters such that `f char`
-- evaluates to 'Nothing'.
collectCharEvents
  :: (MonadIO m)
     => Output a
     -> (Char -> Maybe a)
     -> ContextT GLFWWindow os f m ()
collectCharEvents output f = do
  let callback _ =
        maybe (return ()) (void . atomically . send output) . f
  withContextWindow (\win -> liftIO $ setCharCallback (getGLFWWindow win) (Just callback))
