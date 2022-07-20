{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

  -- In questo programma verranno implementati modelli e texture, e inoltre si parlerà di animazioni
  -- gloss utilizza una funzione chiamata loadBMP, che permette di caricare texture
  -- loadBMP :: FilePath -> IO Picture

  -- Useremo un immagine per il giocatore, e due per i mostri (stato calmo / stato arrabbiato)

import Lib
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Rendering
import System.Exit ( exitSuccess )
import Control.Concurrent (threadDelay)
import Control.Monad (unless, join)
import Control.Monad.Fix (fix)
import FRP.Elerea.Simple
import System.Random

type Pos = Point -- gloss Point type (Float, Float)
newtype Player = Player {position :: Pos}
data Monster = Monster Pos MonsterStatus deriving Show
data MonsterStatus = Wander Direction Int | Hunting deriving Show
data Direction = WalkUp | WalkDown | WalkLeft | WalkRight deriving (Show, Enum, Bounded)

width :: Int
width = 640
height :: Int
height = 480

initialPlayer :: Player
initialPlayer = Player (0,0)
playerSize :: Float
playerSize = 20

initialMonster :: Monster
initialMonster = Monster (100,100) (Wander WalkUp wanderDist)
wanderDist :: Int
wanderDist = 40
monsterSize :: Float
monsterSize = 20
huntingDist :: Float
huntingDist = 100
monsterSpeed :: Float
monsterSpeed = 5

instance Random Direction where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

-- Consigliabile importare subito tutte le texture così da non sovraccaricare poi
main :: IO ()
main = do
  -- solita preparazione dello stato e dei tasti
  (directionKeyGen, directionKeySink) <- external (False, False, False, False)
  randomGenerator <- newStdGen
  glossState <- initState
  -- caricamento textures
  playerTexture <- loadBMP "image/tartaaa.bmp"
  backgroundTexture <- loadBMP "image/background-1.bmp"
  monsterWalkingTexture <- loadBMP "image/monster-walking.bmp"
  monsterHuntingTexture <- loadBMP "image/monster-hunting.bmp"
  let textures = [playerTexture, monsterWalkingTexture, monsterHuntingTexture, backgroundTexture]
  -- creo la finestra
  withWindow width height "Game-Demo" $ \win -> do
            network <- start $ do
              directionKey <- directionKeyGen
              hunted win directionKey randomGenerator textures glossState
            fix $ \loop -> do
                 readInput win directionKeySink
                 join network
                 threadDelay 20000
                 esc <- keyIsPressed win Key'Escape
                 unless esc loop
            exitSuccess

readInput :: Window -> ((Bool, Bool, Bool, Bool) -> IO b) -> IO b
readInput window directionKeySink = do
  pollEvents
  l <- keyIsPressed window Key'Left
  r <- keyIsPressed window Key'Right
  u <- keyIsPressed window Key'Up
  d <- keyIsPressed window Key'Down
  directionKeySink (l,r,u,d)

-- FUNZIONI DI STATO DI GIOCO

hunted win directionKey randomGenerator textures glossState = mdo
  player <- transfer2 initialPlayer (movePlayer 10) directionKey gameOver'
  randomNumber <- stateful (undefined, randomGenerator) nextRandom
  monster <- transfer3 initialMonster wanderOrHunt player randomNumber gameOver'
  gameOver <- memo (playerEaten <$> player <*> monster)
  gameOver' <- delay False gameOver
  return $ renderFrame win glossState textures <$> player <*> monster <*> gameOver
  where playerEaten player monster = distance player monster < (10^2)
        nextRandom (a, g) = random g

-- FUNZIONI DI MOVIMENTO

movePlayer :: Float -> (Bool, Bool, Bool, Bool) -> Bool -> Player -> Player
movePlayer _ _ True player = player
movePlayer increment direction False player
  | outsideOfLimits (position (move direction player increment)) playerSize = player
  | otherwise = move direction player increment


move :: (Bool, Bool, Bool, Bool) -> Player -> Float -> Player
move (True, _, _, _) (Player (xpos, ypos)) increment = Player (xpos - increment, ypos)
move (_, True, _, _) (Player (xpos, ypos)) increment = Player (xpos + increment, ypos)
move (_, _, True, _) (Player (xpos, ypos)) increment = Player (xpos, ypos + increment)
move (_, _, _, True) (Player (xpos, ypos)) increment = Player (xpos, ypos - increment)
move (False, False, False, False) (Player (xpos, ypos)) _ = Player (xpos, ypos)

outsideOfLimits :: (Ord a, Fractional a) => (a, a) -> a -> Bool
outsideOfLimits (xmon, ymon) size = xmon > fromIntegral width/2 - size/2 ||
                                    xmon < (-fromIntegral width/2 + size/2) ||
                                    ymon > fromIntegral height/2 - size/2 ||
                                    ymon < (-fromIntegral height/2 + size/2)



wanderOrHunt :: Player -> (Direction, b) -> Bool -> Monster -> Monster
wanderOrHunt _ _ True monster = monster
wanderOrHunt player (randomDirection, _) False monster =
  if close player monster
  then hunt player monster
  else wander randomDirection monster

close :: Player -> Monster -> Bool
close player monster = distance player monster < huntingDist^2

distance :: Player -> Monster -> Float
distance (Player (xpos, ypos)) (Monster (xmon, ymon)_) = (xpos - xmon)^2 + (ypos - ymon)^2

hunt :: Player -> Monster -> Monster
hunt (Player(xpos, ypos))(Monster(xmon, ymon) _) =
  Monster (xmon + signum (xpos-xmon) * monsterSpeed, ymon + signum (ypos-ymon) * monsterSpeed) Hunting

wander :: Direction -> Monster -> Monster
wander r (Monster(xmon, ymon) (Wander _ 0)) =
  Monster (xmon,ymon) (Wander r wanderDist)
wander r (Monster (xmon, ymon) Hunting) =
  Monster (xmon,ymon) (Wander r wanderDist)
-- Continua dritto
wander _ (Monster (xmon, ymon) (Wander direction n)) = do
                   let currentDirection = continueDirection direction (outsideOfLimits (xmon, ymon) monsterSize)
                   Monster
                       (stepInCurrentDirection currentDirection (xmon, ymon) monsterSpeed)
                       (Wander currentDirection (n-1))

-- Funzione che permette di cambiare direzione ai muri
continueDirection :: Direction -> Bool -> Direction
continueDirection WalkUp True = WalkDown
continueDirection WalkDown True = WalkUp
continueDirection WalkLeft True = WalkRight
continueDirection WalkRight True = WalkLeft
continueDirection dir False = dir


stepInCurrentDirection :: Num a => Direction -> (a, a) -> a -> (a, a)
stepInCurrentDirection WalkUp (xpos, ypos) speed = (xpos, ypos + speed)
stepInCurrentDirection WalkDown (xpos, ypos) speed = (xpos, ypos - speed)
stepInCurrentDirection WalkLeft (xpos, ypos) speed = (xpos - speed, ypos)
stepInCurrentDirection WalkRight (xpos, ypos) speed = (xpos + speed, ypos)

-- FUNZIONI DI DISEGNO

renderFrame window glossState [playerTexture, monsterWalkingTexture, monsterHuntingTexture, backgroundTexture]
  (Player (xpos, ypos)) (Monster (xmon, ymon) status) gameOver = do
    displayPicture (width, height) black glossState 1.0 $ Pictures $ gameOnGoing gameOver [backgroundTexture, 
      renderPlayer xpos ypos playerTexture,
      renderMonster status xmon ymon (monsterWalkingTexture, monsterHuntingTexture)]

renderPlayer :: Float -> Float -> Picture -> Picture
renderPlayer = translate

renderMonster :: MonsterStatus -> Float -> Float -> (Picture, Picture) -> Picture
renderMonster Hunting xpos ypos (_, monsterHuntingTexture) =
  translate xpos ypos monsterHuntingTexture
renderMonster (Wander _ _) xpos ypos (monsterWalkingTexture, _) =
  translate xpos ypos monsterWalkingTexture

gameOnGoing :: Bool -> [Picture] -> [Picture]
gameOnGoing gameOver pics = if gameOver then pics ++ [Color black $ translate (-100) 0 $ Scale 0.3 0.3 $ Text "Game Over"]
  else pics
