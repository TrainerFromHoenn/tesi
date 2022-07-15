{-# LANGUAGE PackageImports #-}

import Control.Concurrent (threadDelay)
import Control.Monad (Functor (fmap), Monad (return), unless, when)
import Data.Bool (Bool (True))
import Data.List (unwords)
import Data.Maybe (Maybe (Just, Nothing))
import Data.String (String)
import Graphics.Gloss (Picture (Color, Pictures), arcSolid, bright, chartreuse, circle, circleSolid, cyan, dark, dim, green, light, line, lineLoop, magenta, makeColor, polygon, rectanglePath, rectangleSolid, rectangleWire, red, rotate, scale, text, thickArc, thickCircle, translate, violet, white, yellow)
import Graphics.Gloss.Interface.IO.Game (Key, KeyState)
import Graphics.Gloss.Rendering (displayPicture, initState)
import Graphics.UI.GLFW (Key (Key'Escape), KeyState (KeyState'Pressed, KeyState'Repeating), Window, pollEvents, swapBuffers, windowHint)
import qualified Graphics.UI.GLFW as GLFW
import "GLFW-b" Graphics.UI.GLFW as GLFW
import System.Exit (exitSuccess)

windowWidth, windowHeight :: Int
windowWidth = 640
windowHeight = 480

-- Funzione conveniente per la creazione di finestre
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

-- Con la funzione withWindow, il main è stupidissimo
-- main :: IO ()
-- main = do
--

-- Creiamo il loop del nostro programma
loop window = do
  pollEvents -- Controlla gli input utente
  renderFrame window -- Renderizza il frame
  threadDelay 20000 -- Permette al sistema di avere il tempo di renderizzare (circa 60fps)
  k <- keyIsPressed window Key'Escape -- Possibile uscita dal loop
  if k
    then return ()
    else loop window

keyIsPressed :: Window -> GLFW.Key -> IO Bool -- Funzione per controllare il tasto premuto
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: GLFW.KeyState -> Bool
isPress KeyState'Pressed = True
isPress KeyState'Repeating = True
isPress _ = False

renderFrame window = do
  -- Funzione per renderizzare
  -- Disegni di vario tipo
  displayPicture (width, height) white glossState 1.0 $
    Pictures
      [ Color violet $
          translate (-300) 100 $
            polygon [(-10, 10), (-10, 70), (20, 20), (20, 30)], -- Poligono viola
        Color red $
          translate (-200) 100 $
            line [(-30, 30), (-40, 30), (30, 40), (50, -20)], -- linea rossa a segmenti
        Color (makeColor 0 128 255 1) $
          translate (-200) 100 $
            lineLoop [(-30, -30), (-40, 30), (30, 40), (50, -20)], -- linea azzurrina a segmenti che si richiude
        Color red $
          translate 100 100 $
            circle 30, -- cerchio rosso
        Color green $
          translate 100 100 $
            thickCircle 30 10, -- cerchio con linea spessa verde
        Color yellow $
          translate 200 100 $
            circleSolid 30, -- cerchio pieno giallo
        Color chartreuse $
          translate (-200) (-100) $
            thickArc 0 180 30 30, -- arco verdognolo
        Color (dark magenta) $
          translate (-100) (-100) $
            arcSolid 0 90 30, -- arco pieno magenta
        Color (bright magenta) $
          translate 0 (-100) $
            scale 0.2 0.2 $
              text "Boo!", -- testo piccolo magenta
        Color (dim cyan) $ translate 100 (-100) $ rotate 30 $ rectangleWire 20 50,
        Color (light cyan) $ translate 200 (-100) $ rotate 60 $ rectangleSolid 20 50 -- rettangolo pieno ciano
      ]

  swapBuffers window -- Scambia frame vecchio con frame nuovo
