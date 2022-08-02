module Main where

import Graphics.Gloss
import Game
import Rendering
import Logic
import System.Random

window :: Display
window = InWindow "Game" (640,480) (100, 100)
backgroundColor :: Color
backgroundColor = makeColor 0 0 0 0


main :: IO ()
main = do
  g <- newStdGen
  initialGame <- loadWorld g
  play window backgroundColor 30 initialGame gameAsPicture inputFunction updateFunction
  



