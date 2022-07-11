module Main where

import Lib
import "GLFW-b" Graphics.Ui.GLFW as GLFW

-- Funzione che permette la gestione della finestra, non serve ricordarla
withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
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

main :: IO ()
main = do
    let width = 640
        height = 480
    withWindow width height "Resurrection" $ \win -> do 
        loop win

-- Implementazione di un loop
loop window = do 
    pollEvents -- cerca input
    renderFrame window -- renderizza il prossimo frame (definizione sotto)
    threadDelay 2000 -- Do il tempo a OpenGL di renderizzare completamente il frame (2000 microsecondi)
    -- La questione fps costanti è roba da malati ed è inutile fregarmene ora
    k <- keyIsPressed window Key'Escape -- possibile uscita dal loop (definizione di keyIsPressed sotto)
    if k 
        then return ()
        else loop window

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed = True
isPress KeyState'Repeating = True
isPress _ = False

-- Per disegnare si usa la funzione displayPicture di Gloss
-- Input: larghezza/altezza finestra, colore, rendering state, view port, disegno vero e proprio
-- Output: azioni IO

renderFrame window = do
    -- qua si disegna
    swapBuffers window

