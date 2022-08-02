module Rendering where

import Graphics.Gloss
import Game

gameAsPicture :: Game -> Picture
gameAsPicture world = frame
  where frame = case gameState world of
                  Running -> runningPicture world
                  GameOver -> youDied (gameTextures world)

runningPicture :: Game -> Picture
runningPicture world = Pictures [background textures,
                                 renderPlayer (gamePlayer world) (player textures),
                                 renderMonster (gameMonster world) (monsterWalking textures) (monsterHunting textures)]
                        where textures = gameTextures world

renderPlayer :: Player -> TextureSet -> Picture
renderPlayer playerInfo texture =
  case dir playerInfo of
    Just WalkUp -> translate xpos ypos $ back texture
    Just WalkDown -> translate xpos ypos $ front texture
    Just WalkRight -> translate xpos ypos $ right texture
    Just WalkLeft -> translate xpos ypos $ left texture
    Nothing -> translate xpos ypos $ front texture
    where (xpos, ypos) = position playerInfo

renderMonster :: Monster -> TextureSet -> TextureSet -> Picture
renderMonster monsterInfo walkTextures huntTextures =
  case status monsterInfo of
    Hunting WalkUp -> translate xpos ypos $ back huntTextures
    Hunting WalkDown -> translate xpos ypos $ front huntTextures
    Hunting WalkLeft -> translate xpos ypos $ left huntTextures
    Hunting WalkRight -> translate xpos ypos $ right huntTextures
    Wander WalkUp _ -> translate xpos ypos $ back walkTextures
    Wander WalkDown _ -> translate xpos ypos $ front walkTextures
    Wander WalkLeft _ -> translate xpos ypos $ left walkTextures
    Wander WalkRight _ -> translate xpos ypos $ right walkTextures
    where (xpos, ypos) = monsterPos monsterInfo


