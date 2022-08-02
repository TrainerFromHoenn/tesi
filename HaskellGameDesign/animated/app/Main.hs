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
import Control.Applicative ((<*>), (<$>))
import FRP.Elerea.Simple
import System.Random

type Pos = Point -- gloss Point type (Float, Float)
data Monster = Monster Pos MonsterStatus deriving Show
data MonsterStatus = Wander Direction Int | Hunting Direction deriving Show
data Direction = WalkUp | WalkDown | WalkLeft | WalkRight deriving (Show, Enum, Bounded)

-- Voglio animare le texture, quindi creo un nuovo dato
data TextureSet = TextureSet {
  front :: Picture,
  back :: Picture,
  left :: Picture,
  right :: Picture
}

data Textures = Textures {
  background :: Picture,
  player :: TextureSet,
  monsterWalking :: TextureSet,
  monsterHunting :: TextureSet,
  youDied :: Picture
}
-- inoltre per dare indicazione al programma della direzione dei nostri personaggi, è necessario
-- dare informazioni aggiuntive sul player
data Player = Player {
  position :: Pos,
  dir :: Maybe Direction}
              deriving Show

width :: Int
width = 640
height :: Int
height = 480

initialPlayer :: Player
initialPlayer = Player (0,0) (Just WalkDown)
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
loadTextures :: IO Textures
loadTextures = do
  playerTextureSet <- TextureSet <$> loadBMP "image/knight-front.bmp"
                                 <*> loadBMP "image/knight-back.bmp"
                                 <*> loadBMP "image/knight-left.bmp"
                                 <*> loadBMP "image/knight-right.bmp"
  monsterWalkingSet <- TextureSet <$> loadBMP "image/monster-walking-front.bmp"
                                  <*> loadBMP "image/monster-walking-back.bmp"
                                  <*> loadBMP "image/monster-walking-left.bmp"
                                  <*> loadBMP "image/monster-walking-right.bmp"
                                  -- In questo caso solo left/right, si muove in diagonale!
  monsterHuntingSet <- TextureSet <$> loadBMP "image/monster-hunting-left.bmp"
                                  <*> loadBMP "image/monster-hunting-right.bmp"
                                  <*> loadBMP "image/monster-hunting-left.bmp"
                                  <*> loadBMP "image/monster-hunting-right.bmp"
  backgroundTexture <- loadBMP "image/background-1.bmp"
  gameOverTexture <- loadBMP "image/youdied.bmp"
  return Textures { background = backgroundTexture,
                    player = playerTextureSet,
                    monsterWalking = monsterWalkingSet,
                    monsterHunting = monsterHuntingSet,
                    youDied = gameOverTexture }

main :: IO ()
main = do
  -- solita preparazione dello stato e dei tasti
  (directionKeyGen, directionKeySink) <- external (False, False, False, False)
  randomGenerator <- newStdGen
  glossState <- initState
  -- caricamento textures
  textures <- loadTextures
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
move (True, _, _, _) (Player (xpos, ypos) _) increment = Player (xpos - increment, ypos) $ Just WalkLeft
move (_, True, _, _) (Player (xpos, ypos) _) increment = Player (xpos + increment, ypos) $ Just WalkRight
move (_, _, True, _) (Player (xpos, ypos) _) increment = Player (xpos, ypos + increment) $ Just WalkUp
move (_, _, _, True) (Player (xpos, ypos)_) increment = Player (xpos, ypos - increment) $ Just WalkDown
move (False, False, False, False) (Player (xpos, ypos) _ ) _ = Player (xpos, ypos) Nothing

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
distance (Player (xpos, ypos) _) (Monster (xmon, ymon)_) = (xpos - xmon)^2 + (ypos - ymon)^2

hunt :: Player -> Monster -> Monster
hunt (Player(xpos, ypos) _ )(Monster(xmon, ymon) _) =
  Monster (xmon + signum (xpos-xmon) * monsterSpeed, ymon + signum (ypos-ymon) * monsterSpeed)
          (Hunting $ huntingDirection (signum (xpos - xmon)) (signum (ypos - ymon)))

huntingDirection (-1) (-1) = WalkLeft
huntingDirection (-1) 1 = WalkLeft
huntingDirection 1 (-1) = WalkRight
huntingDirection 1 1 = WalkRight
huntingDirection _ _ = WalkRight

wander :: Direction -> Monster -> Monster
wander r (Monster(xmon, ymon) (Wander _ 0)) =
  Monster (xmon,ymon) (Wander r wanderDist)
wander r (Monster (xmon, ymon) (Hunting _)) =
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
renderFrame :: Window -> State -> Textures -> Player -> Monster -> Bool -> IO()
renderFrame window glossState textures
  (Player (xpos, ypos) playerDir) (Monster (xmon, ymon) status) gameOver = do
    displayPicture (width, height) black glossState 1.0 $ Pictures $ gameOnGoing gameOver
     [background textures,
      renderPlayer xpos ypos playerDir (player textures),
      renderMonster status xmon ymon (monsterWalking textures) (monsterHunting textures),
      youDied textures]
    swapBuffers window

renderPlayer :: Float -> Float -> Maybe Direction -> TextureSet -> Picture
renderPlayer xpos ypos (Just WalkUp) textureSet =
  translate xpos ypos $ back textureSet
renderPlayer xpos ypos (Just WalkDown) textureSet =
  translate xpos ypos $ front textureSet
renderPlayer xpos ypos (Just WalkRight) textureSet =
  translate xpos ypos $ right textureSet
renderPlayer xpos ypos (Just WalkLeft) textureSet =
  translate xpos ypos $ left textureSet
renderPlayer xpos ypos Nothing textureSet =
  translate xpos ypos $ front textureSet

renderMonster :: MonsterStatus -> Float -> Float -> TextureSet -> TextureSet -> Picture
renderMonster (Hunting WalkLeft) xpos ypos _ textureSet =
  translate xpos ypos $ left textureSet
renderMonster (Hunting WalkRight) xpos ypos _ textureSet =
  translate xpos ypos $ right textureSet
renderMonster (Wander WalkUp _) xpos ypos textureSet _ =
  translate xpos ypos $ back textureSet
renderMonster (Wander WalkDown _) xpos ypos textureSet _ =
  translate xpos ypos $ front textureSet
renderMonster (Wander WalkLeft _) xpos ypos textureSet _ =
  translate xpos ypos $ left textureSet
renderMonster (Wander WalkRight _) xpos ypos textureSet _ =
  translate xpos ypos $ right textureSet

gameOnGoing :: Bool -> [Picture] -> [Picture]
gameOnGoing gameOver pics =
  if gameOver then Prelude.init pics ++ [last pics]
  else Prelude.init pics
