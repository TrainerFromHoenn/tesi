module Game where
import Graphics.Gloss
import qualified Data.Set as S
import Graphics.Gloss.Interface.Pure.Game (Key)
import System.Random


data Game = Game {
  gameTextures :: Textures,
  gamePlayer :: Player,
  gameMonster :: Monster,
  gameState :: State,
  generator :: StdGen,
  keys :: S.Set Key -- Teniamo traccia dei tasti premuti
} deriving (Eq, Show)

type Pos = Point
data Monster = Monster {
  monsterPos :: Pos,
  status :: MonsterStatus
} deriving (Eq, Show)

data MonsterStatus = Wander Direction Int | Hunting Direction deriving (Eq, Show)
-- o cammina in una direzione a velocità x, o caccia in una direzione (la velocità è diversa)
data Direction = WalkUp | WalkDown | WalkLeft | WalkRight deriving (Eq, Show, Enum, Bounded)
data Player = Player {
  position :: Pos,
  dir :: Maybe Direction
} deriving (Eq, Show)
data State = Running | GameOver deriving (Eq, Show)

data TextureSet = TextureSet {
  front :: Picture,
  back :: Picture,
  left :: Picture,
  right :: Picture
} deriving (Eq, Show)

data Textures = Textures {
    background :: Picture,
    player :: TextureSet,
    monsterWalking :: TextureSet,
    monsterHunting :: TextureSet,
    youDied :: Picture
} deriving (Eq, Show)


-- Stato iniziale
initialPlayer :: Player
initialPlayer = Player (0,0) (Just WalkDown)
initialMonster :: Monster
initialMonster = Monster (100, 100) (Wander WalkDown wanderDist)
playerSize :: Float
playerSize = 20
wanderDist :: Int
wanderDist = 40
monsterSize :: Float
monsterSize = 20
huntingDist :: Float
huntingDist = 10000
monsterSpeed :: Float
monsterSpeed = 5
playerSpeed :: Float
playerSpeed = 10

-- Schermo
width :: Int
width = 640
height :: Int
height = 480

loadWorld :: StdGen -> IO Game
loadWorld g = do
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
  let loadedTextures = Textures {
    background = backgroundTexture,
    player = playerTextureSet,
    monsterWalking = monsterWalkingSet,
    monsterHunting = monsterHuntingSet,
    youDied = gameOverTexture
  }
  return Game {
      gameTextures = loadedTextures,
      gamePlayer = initialPlayer,
      gameMonster = initialMonster,
      gameState = Running,
      generator = g,
      keys = S.empty -- Iniziamo con 0 tasti premuti
  }
