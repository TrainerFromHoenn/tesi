module Main where

-- base

import Control.Exception (bracket)
import Control.Monad (when)
-- gl in generale
import Graphics.GL.Core33
import Graphics.GL.Types
import qualified Graphics.UI.GLFW as GLFW
import Lib

minWidth = 800

minHeight = 600

winTitle = "Hello Window"

-- ora definisco il callback per ogni volta che premo un tasto
-- in prima battuta stampiamo solo il tasto premuto
callback :: GLFW.KeyCallback
callback window key scanCode keyState modKeys = do
  print key
  when
    (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
    (GLFW.setWindowShouldClose window True)

-- nel main invece inizializziamo GLFW, con i nostri suggerimenti e poi renderizziamo uno schermo nero

main :: IO ()
main = bracketGLFW $ do
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  GLFW.windowHint (GLFW.WindowHint'Resizable False)
  maybeWindow <- GLFW.createWindow minWidth minHeight winTitle Nothing Nothing
  case maybeWindow of
    Nothing -> putStrLn "Failed to create a GLFW window!"
    Just window -> do
      -- enable keys
      GLFW.setKeyCallback window (Just callback)
      -- calibrate the viewport
      GLFW.makeContextCurrent (Just window)
      (x, y) <- GLFW.getFramebufferSize window
      glViewport 0 0 (fromIntegral x) (fromIntegral y)
      -- enter our main loop
      let loop = do
            shouldContinue <- not <$> GLFW.windowShouldClose window
            when shouldContinue $ do
              -- event poll
              GLFW.pollEvents
              -- drawing
              glClearColor 0.2 0.3 0.3 1.0
              glClear GL_COLOR_BUFFER_BIT
              -- swap buffers and go again
              GLFW.swapBuffers window
              loop
      loop

bracketGLFW :: IO () -> IO ()
bracketGLFW act = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
  when initWorked act
