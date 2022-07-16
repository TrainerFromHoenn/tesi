{-# LANGUAGE PackageImports #-}

module Lib
  ( withWindow,
    keyIsPressed,
  )
where

import Control.Monad (Functor (fmap), Monad (return), unless, when)
import Graphics.Gloss (Picture (Color, Pictures), arcSolid, bright, chartreuse, circle, circleSolid, cyan, dark, dim, green, light, line, lineLoop, magenta, makeColor, polygon, rectanglePath, rectangleSolid, rectangleWire, red, rotate, scale, text, thickArc, thickCircle, translate, violet, white, yellow)
import Graphics.Gloss.Interface.IO.Game (Key, KeyState)
import Graphics.Gloss.Rendering (displayPicture, initState)
import Graphics.UI.GLFW (Key (Key'Escape), KeyState (KeyState'Pressed, KeyState'Repeating), Window, pollEvents, swapBuffers, windowHint)
import qualified Graphics.UI.GLFW as GLFW
import "GLFW-b" Graphics.UI.GLFW as GLFW

withWindow :: Int -> Int -> String -> (Window -> IO ()) -> IO ()
withWindow width height title f = do
  setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      (Just win) -> do
        GLFW.makeContextCurrent m
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
      Nothing -> return ()
    GLFW.terminate
  where
    simpleErrorCallback e s =
      putStrLn $ unwords [show e, show s]

keyIsPressed :: Window -> GLFW.Key -> IO Bool -- Funzione per controllare il tasto premuto
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: GLFW.KeyState -> Bool
isPress KeyState'Pressed = True
isPress KeyState'Repeating = True
isPress _ = False
