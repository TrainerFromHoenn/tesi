module Logic where
import Game
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as S
import System.Random

inputFunction :: Event -> Game -> Game
inputFunction (EventKey k Down _ _) world = world {keys = S.insert k (keys world)} -- Tasto premuto
inputFunction (EventKey k Up _ _) world = world {keys = S.delete k (keys world)} -- Tasto rilasciato
inputFunction _ world = world -- Nessun tasto premuto

updateFunction :: Float -> Game -> Game
updateFunction _ world =
  if checkForDeath world
  then world { gameState = GameOver }
  else movementDetection world

checkForDeath :: Game -> Bool
checkForDeath world
  | distance warrior monster < 100 = True
  | otherwise = False
  where warrior = gamePlayer world
        monster = gameMonster world

distance :: Player -> Monster -> Float
distance (Player (xpos, ypos) _) (Monster (xmon, ymon) _) = (xpos - xmon)^2 + (ypos - ymon)^2

movementDetection :: Game -> Game
movementDetection world
   | S.member (SpecialKey KeyUp) (keys world) = moveWorld world (Just WalkUp)
   | S.member (SpecialKey KeyDown) (keys world) = moveWorld world (Just WalkDown)
   | S.member (SpecialKey KeyLeft) (keys world) = moveWorld world (Just WalkLeft)
   | S.member (SpecialKey KeyRight) (keys world) = moveWorld world (Just WalkRight)
   | otherwise = moveWorld world Nothing

moveWorld :: Game -> Maybe Direction -> Game
moveWorld world direction = world {
  gamePlayer = movePlayer playerSpeed direction pl,
  generator = finalGenerator,
  gameMonster = wanderOrHunt mons pl (toDir randomDir) -- randomDirection
} where pl = gamePlayer world
        mons = gameMonster world
        g0 = generator world
        (randomDir, g1) = randomDirection g0
        finalGenerator = g1

movePlayer :: Float -> Maybe Direction -> Player -> Player
movePlayer _ Nothing pl = pl
movePlayer increment direction pl
  | outsideOfLimits (position (move direction pl increment)) playerSize = pl -- Controllo se il movimento sposterebbe il player
                                                                             -- fuori dallo schermo
  | otherwise = move direction pl increment

outsideOfLimits :: Pos -> Float -> Bool
outsideOfLimits (x, y) size = x > fromIntegral width/2 - size/2 ||
                              x < (-fromIntegral width/2 + size/2) ||
                              y > fromIntegral height/2 - size/2 ||
                              y < (-fromIntegral height/2 + size/2)

move :: Maybe Direction -> Player -> Float -> Player
move (Just WalkUp) (Player (xpos, ypos) _) increment = Player (xpos, ypos + increment) $ Just WalkUp
move (Just WalkLeft) (Player (xpos, ypos) _) increment = Player (xpos - increment, ypos) $ Just WalkLeft
move (Just WalkDown) (Player (xpos, ypos) _) increment = Player (xpos, ypos  - increment) $ Just WalkDown
move (Just WalkRight) (Player (xpos, ypos) _) increment = Player (xpos + increment, ypos) $ Just WalkRight
move _ (Player (xpos, ypos) direction) _ = Player (xpos, ypos) direction

wanderOrHunt :: Monster -> Player -> Direction -> Monster
wanderOrHunt monster pl randomDir =
  if close pl monster
  then hunt pl monster
  else wander randomDir monster

close :: Player -> Monster -> Bool
close pl monster = distance pl monster < huntingDist

hunt :: Player -> Monster -> Monster
hunt (Player (xpos, ypos) _) (Monster (xmon, ymon) _) =
  Monster (xmon + signum (xpos-xmon) * monsterSpeed, ymon + signum (ypos-ymon) * monsterSpeed)
          (Hunting $ huntingDirection (signum (xpos - xmon)) (signum (ypos - ymon)))

huntingDirection :: Float -> Float -> Direction -- il mostro si muove solo in diagonale quando caccia
huntingDirection (-1) (-1) = WalkLeft
huntingDirection (-1) 1 = WalkLeft
huntingDirection 1 (-1) = WalkRight
huntingDirection 1 1 = WalkRight
huntingDirection _ _ = WalkRight

wander :: Direction -> Monster -> Monster
wander r (Monster (xmon, ymon) (Wander _ 0)) = -- Caso base, mostro fermo
  Monster (xmon, ymon) (Wander r wanderDist)
wander r (Monster (xmon, ymon) (Hunting _)) = -- Se stava cacciando nel frame prima ma ora è in wander
  Monster (xmon, ymon) (Wander r wanderDist)
wander _ (Monster (xmon, ymon) (Wander direction n)) = -- Caso del "continua dritto"
  Monster (stepInCurrentDirection currentDirection (xmon, ymon) monsterSpeed) (Wander currentDirection (n-1))
  where currentDirection = continueDirection direction (outsideOfLimits (xmon, ymon) monsterSize)

stepInCurrentDirection :: Direction -> Pos -> Float -> Pos
stepInCurrentDirection WalkUp (xpos, ypos) speed = (xpos, ypos + speed)
stepInCurrentDirection WalkDown (xpos, ypos) speed = (xpos, ypos - speed)
stepInCurrentDirection WalkLeft (xpos, ypos) speed = (xpos - speed, ypos)
stepInCurrentDirection WalkRight (xpos, ypos) speed = (xpos + speed, ypos)

continueDirection :: Direction -> Bool -> Direction
continueDirection WalkUp True = WalkDown -- Se ti schianti inverti direzione
continueDirection WalkDown True = WalkUp
continueDirection WalkLeft True = WalkRight
continueDirection WalkRight True = WalkLeft
continueDirection direction False = direction

randomDirection :: StdGen -> (Int, StdGen)
randomDirection = randomR (1, 4)

toDir :: Int -> Direction
toDir 1 = WalkUp
toDir 2 = WalkDown
toDir 3 = WalkRight
toDir 4 = WalkLeft
toDir _ = WalkDown