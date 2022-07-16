{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.State.Strict
import Graphics.Gloss
import Graphics.Gloss.Rendering as RS
import Graphics.UI.GLFW (Key (..), Window, pollEvents, swapBuffers)
import Lib (keyIsPressed, withWindow)
import System.Exit (ExitCode (ExitSuccess), exitSuccess, exitWith)

-- Voglio creare un programma che tenga conto dello stato del gioco, per esempio posizione giocatore, salute, ecc

type Pos = Point -- tipo definito da Gloss (Float, Float)

data Player = Player {position :: Pos} -- Un giocatore ha una posizione, di tipo Pos (X,Y)

initialPlayer = Player (200, 200)

playerSize = 20

width = 640

height = 480

main :: IO ()
main = do
  glossState <- initState
  withWindow width height "Game-Demo" $ \win -> do
    -- Uso lo state T, un costrutto
    _ <- runStateT (loop win glossState) initialPlayer
    exitSuccess

loop :: Window -> RS.State -> StateT Player IO ()
loop window glossState = do
  lift $ threadDelay 20000
  lift pollEvents
  k <- lift $ keyIsPressed window Key'Escape
  l <- lift $ keyIsPressed window Key'Left
  r <- lift $ keyIsPressed window Key'Right
  u <- lift $ keyIsPressed window Key'Up
  d <- lift $ keyIsPressed window Key'Down
  player <- get
  let newState = movePlayer (l, r, u, d) player 10
  put newState
  lift $ renderFrame newState window glossState
  unless k $ loop window glossState
  where
    renderFrame (Player (xpos, ypos)) window glossState = do
      displayPicture (width, height) white glossState 1.0 $ translate xpos ypos $ rectangleSolid playerSize playerSize
      swapBuffers window

movePlayer :: (Bool, Bool, Bool, Bool) -> Player -> Float -> Player
movePlayer direction player increment
  | outsideOfLimits (position (move direction player increment)) playerSize = player
  | otherwise = move direction player increment

-- Siamo fuori dai limiti se superiamo il size della finestra
outsideOfLimits :: (Float, Float) -> Float -> Bool
outsideOfLimits (xmon, ymon) size =
  xmon > fromIntegral width / 2 - size / 2
    || xmon < (-fromIntegral width / 2 + size / 2)
    || ymon > fromIntegral height / 2 - size / 2
    || ymon < (-fromIntegral height / 2 + size / 2)

move (True, _, _, _) (Player (xpos, ypos)) increment =
  -- LEFT
  Player (xpos - increment, ypos)
move (_, True, _, _) (Player (xpos, ypos)) increment =
  -- RIGHT
  Player (xpos + increment, ypos)
move (_, _, True, _) (Player (xpos, ypos)) increment =
  -- UP
  Player (xpos, ypos + increment)
move (_, _, _, True) (Player (xpos, ypos)) increment =
  -- DOWN
  Player (xpos, ypos - increment)
move (_, _, _, _) (Player (xpos, ypos)) increment =
  -- NOTHING
  Player (xpos, ypos)
