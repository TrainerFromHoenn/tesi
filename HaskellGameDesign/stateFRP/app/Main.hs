{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Lib
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Rendering
import Control.Monad.Except (fix, join, unless)
import Control.Concurrent (threadDelay)
import System.Exit (exitSuccess)
import FRP.Elerea.Simple
import System.Random
import Graphics.Gloss.Data.Color (white, black, red, green)
import Graphics.Gloss.Data.Picture (translate, rectangleSolid, circleSolid)

type Pos = Point -- gloss Point type (Float, Float)
newtype Player = Player {position :: Pos}

-- Definisco un mostro con una posizione, uno status (caccia, o si muove nelle 4 direzioni)
-- Nel suo caso voglio un comportamento casuale
data Monster = Monster Pos MonsterStatus deriving Show
data MonsterStatus = Wander Direction Int | Hunting deriving Show
data Direction = WalkUp | WalkDown | WalkLeft | WalkRight deriving (Show, Enum, Bounded)
instance Random Direction where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

-- costanti
initialPlayer :: Player
initialPlayer = Player (0,0)
playerSize :: Float
playerSize = 20
wanderDist :: Int
wanderDist = 40
huntingDist :: Float
huntingDist = 100
initialMonster :: Monster
initialMonster = Monster (200, 200) (Wander WalkUp wanderDist)
width :: Int
width = 640
height :: Int
height = 480
monsterSize :: Float
monsterSize = 20
monsterSpeed :: Float
monsterSpeed = 5


main :: IO ()
main = do
    (directionKeyGen, directionKeySink) <- external (False, False, False, False)
    randomGenerator <- newStdGen
    glossState <- initState

    withWindow width height "Game-Demo" $ \win -> do
          network <- start $ do
            directionKey <- directionKeyGen
            hunted win directionKey randomGenerator glossState
          fix $ \loop -> do
               readInput win directionKeySink
               join network
               threadDelay 20000
               esc <- keyIsPressed win Key'Escape
               unless esc loop
          exitSuccess

hunted win directionKey randomGenerator glossState = mdo
    player <- transfer2 initialPlayer (movePlayer 10) directionKey gameOver'
    randomNumber <- stateful (undefined, randomGenerator) nextRandom
    monster <- transfer3 initialMonster wanderOrHunt player randomNumber gameOver'
    gameOver <- memo (playerEaten <$> player <*> monster)
    gameOver' <- delay False gameOver
    return $ renderFrame win glossState <$> player <*> monster <*> gameOver
    where playerEaten player monster = distance player monster < (10^2  :: Float)
          nextRandom (a, g) = random g


readInput :: Window -> ((Bool, Bool, Bool, Bool) -> IO b) -> IO b
readInput window directionKeySink = do
  pollEvents
  l <- keyIsPressed window Key'Left
  r <- keyIsPressed window Key'Right
  u <- keyIsPressed window Key'Up
  d <- keyIsPressed window Key'Down
  directionKeySink (l,r,u,d)


-- Movimento giocatore
movePlayer :: Float -> (Bool, Bool, Bool, Bool) -> Bool -> Player -> Player
movePlayer _ _ True player = player
movePlayer increment direction False player
         | outsideOfLimits (position (move direction player increment)) playerSize = player
         | otherwise = move direction player increment

outsideOfLimits :: (Float, Float) -> Float -> Bool
outsideOfLimits (xmon, ymon) size = xmon > fromIntegral width/2 - size/2 ||
                                    xmon < (-fromIntegral width/2 + size/2) ||
                                    ymon > fromIntegral height/2 - size/2 ||
                                    ymon < (-fromIntegral height/2 + size/2)

move :: (Bool, Bool, Bool, Bool) -> Player -> Float -> Player
move (True, _, _, _) (Player (xpos, ypos)) increment = Player (xpos - increment, ypos)
move (_, True, _, _) (Player (xpos, ypos)) increment = Player (xpos + increment, ypos)
move (_, _, True, _) (Player (xpos, ypos)) increment = Player (xpos, ypos + increment)
move (_, _, _, True) (Player (xpos, ypos)) increment = Player (xpos, ypos - increment)
move (False, False, False, False) (Player (xpos, ypos)) _ = Player (xpos, ypos)

-- Funzione che decide se il mostro caccia o si muove a caso
wanderOrHunt :: Player -> (Direction, b) -> Bool -> Monster -> Monster
wanderOrHunt _ _ True monster = monster
wanderOrHunt player (randomDirection, _) False monster =
  if close player monster
  then hunt player monster
  else wander randomDirection monster

close :: Player -> Monster -> Bool
close player monster = distance player monster < huntingDist^2

distance :: Player -> Monster -> Float
distance (Player(xpos, ypos)) (Monster(xmon, ymon) _) =
  (xpos - xmon)^2 + (ypos - ymon)^2

-- Funzioni di caccia
hunt :: Player -> Monster -> Monster
hunt (Player(xpos, ypos)) (Monster(xmon, ymon)_) =
  Monster (xmon + signum (xpos-xmon) *monsterSpeed, ymon+signum (ypos-ymon)*monsterSpeed) Hunting

-- Wandering: o cambi direzione a caso, o continui dritto
wander :: Direction -> Monster -> Monster
wander r (Monster(xmon, ymon) (Wander _ 0)) =
  Monster (xmon, ymon) (Wander r wanderDist)
wander r (Monster (xmon, ymon) Hunting) =
  Monster (xmon, ymon) (Wander r wanderDist)
-- Continua dritto
wander _ (Monster (xmon, ymon) (Wander direction n)) = do
                   let currentDirection = continueDirection direction (outsideOfLimits (xmon, ymon) monsterSize)
                   Monster
                       (stepInCurrentDirection currentDirection (xmon, ymon) monsterSpeed)
                       (Wander currentDirection (n-1))

continueDirection :: Direction -> Bool -> Direction
continueDirection WalkUp True = WalkDown
continueDirection WalkDown True = WalkUp
continueDirection WalkLeft True = WalkRight
continueDirection WalkRight True = WalkLeft
continueDirection direction False = direction

stepInCurrentDirection :: Num a => Direction -> (a, a) -> a -> (a, a)
stepInCurrentDirection WalkUp (xpos, ypos) speed = (xpos, ypos + speed)
stepInCurrentDirection WalkDown (xpos, ypos) speed = (xpos, ypos - speed)
stepInCurrentDirection WalkLeft (xpos, ypos) speed = (xpos - speed, ypos)
stepInCurrentDirection WalkRight (xpos, ypos) speed = (xpos + speed, ypos)

-- Rendering
renderFrame :: Window -> State -> Player -> Monster -> Bool -> IO()
renderFrame window glossState (Player(xpos, ypos)) (Monster(xmon, ymon) status) gameOver = do
  displayPicture (width, height) white glossState 1.0 $
    Pictures $ gameOngoing gameOver [renderPlayer xpos ypos, renderMonster status xmon ymon]
  swapBuffers window

renderPlayer :: Float -> Float -> Picture
renderPlayer xpos ypos =
  Color black $ translate xpos ypos $ rectangleSolid playerSize playerSize

renderMonster :: MonsterStatus -> Float -> Float -> Picture
renderMonster status xpos ypos =
  Color (monsterColor status) $ translate xpos ypos $ circleSolid playerSize
    where monsterColor Hunting = red
          monsterColor (Wander _ _) = green

gameOngoing :: Bool -> [Picture] -> [Picture]
gameOngoing gameOver pics = if gameOver then
  pics ++ [Color black $ translate (-100) 0 $ Scale 0.3 0.3 $ Text "Game Over"]
  else pics

-- Se voglio però implementare più livelli del gioco, devo fare in modo che alcuni segnali a una certa vengano
-- eliminati, in modo da creare da 0 un nuovo set di segnali per formare lo stato
-- 2 segnali nuovi: - stato del livello
--                  - dobbiamo procedere o no?







