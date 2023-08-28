{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Config
-- REMOVER DEPOIS DE TESTAR!!!!!!!!!!!!!

import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as M
import Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Renderer
  ( renderAttackAnimation,
    renderEnemies,
    renderEnemyProjectiles,
    renderHUD,
    renderMap,
    renderPlayer,
    renderPossibleMoves,
    screenPositionToWorldPosition,
  )
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import Update (updateAttackAnimation, updateGameMap, updateWorld)

render :: State World -> Picture
render state = pictures [scale scalingFactor scalingFactor $ pictures renderAll, renderHUD state]
  where
    renderAll =
      map
        (\f -> f state)
        [ renderMap,
          renderPossibleMoves,
          renderPlayer,
          renderEnemies,
          renderAttackAnimation,
          renderEnemyProjectiles
        ]

handleEvents :: Event -> State World -> State World
handleEvents (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) state = case mode of
  MoveMode walkableTiles ->
    if (x, y) `elem` walkableTiles
      then state {lastMousePosition = (x, y), playerAction = Move (x - px, y - py)}
      else state {lastMousePosition = (x, y)}
  AttackMode possibleAttackTiles ->
    if (x, y) `elem` possibleAttackTiles
      then state {lastMousePosition = (x, y), playerAction = Attack (x, y), showAttackAnimation = True}
      else state {lastMousePosition = (x, y)}
  _ -> state
  where
    (x, y) = screenPositionToWorldPosition (mouseX, mouseY)
    world = sData state
    mode = wMode world
    player = wPlayer world
    playerEntity = pEnt player
    (px, py) = ePos playerEntity
handleEvents (EventKey key keyState _ _) state
  | updateTimer state < updateInterval = state
  | key == Char 'n' && keyState == Down = state {sData = world {wCurrentMap = (wCurrentMap world + 1) `mod` length (wMaps world)}}
  | key == Char 'm' && keyState == Down =
      if isMoveMode
        then state {sData = world {wMode = NoMode}}
        else state {sData = world {wMode = MoveMode walkableTilesInMoveRange}}
  | key == Char 'a' && keyState == Down =
      if isAttackMode
        then state {sData = world {wMode = NoMode}}
        else state {sData = world {wMode = AttackMode walkableTilesInAttackRange}}
  | otherwise = state
  where
    world = sData state
    isMoveMode = case wMode world of
      MoveMode _ -> True
      _ -> False

    isAttackMode = case wMode world of
      AttackMode _ -> True
      _ -> False

    currentMap = (!! wCurrentMap world) $ wMaps world
    player = wPlayer world
    (px, py) = ePos $ pEnt player
    maxMoveDistance = fromIntegral $ pMaxMoveDistance player
    walkableTilesInMoveRange = filter (not . isEntityInTile world) (findWalkableTilesInDistance currentMap (px, py) maxMoveDistance)

    maxAttackDistance = fromIntegral $ pMaxAttackDistance player
    walkableTilesInAttackRange = findWalkableTilesInDistance currentMap (px, py) maxAttackDistance
handleEvents _ state = state

update :: Float -> State World -> State World
update dt state
  | life (eStats playerEnt) <= 0 = undefined
  | isValidAction action = state {sData = updateWorld dt state, updateTimer = 0, playerAction = NoAction}
  | showAttackAnimation state && updateTimer state >= updateInterval = state {showAttackAnimation = False, updateTimer = 0, sData = world}
  | null $ wEnemies world = state {sData = world {wEnemies = newEnemies, wCurrentMap = (wCurrentMap world + 1) `mod` length (wMaps world)}}
  | otherwise = state {updateTimer = curUpdateTimer, sData = world}
  where
    world = sData state
    playerEnt = pEnt $ wPlayer $ sData state
    action = playerAction state
    curUpdateTimer = updateTimer state + dt
    mapIndex = wCurrentMap world + 1
    map' = (!! mapIndex) $ wMaps world
    newEnemies = getMapEnemies state map'

isFile :: FilePath -> Bool
isFile path = path /= "." && path /= ".."

main :: IO ()
main =
  do
    tilesContent <- getDirectoryContents tilesFolder
    let tiles = map (tilesFolder </>) $ filter isFile tilesContent

    gameTiles <- mapM loadTile tiles
    tilePictures <- mapM (loadBMP . tTexture) gameTiles

    let zippedTiles = zip (map tTexture gameTiles) tilePictures
    let tileMap = M.fromList zippedTiles

    mapsContent <- getDirectoryContents mapsFolder
    let maps = map (mapsFolder </>) $ filter isFile mapsContent

    charMaps <- mapM loadMap maps
    let gameMaps = createMaps charMaps gameTiles

    playerPicture <- loadBMP (charactersFolder </> "knight.bmp")

    enemyPicture1 <- loadBMP (charactersFolder </> "ogre.bmp")
    enemyPicture2 <- loadBMP (charactersFolder </> "mage.bmp")

    swordPicture <- loadBMP (itemsFolder </> "sword.bmp")
    fireballPicture <- loadBMP (objectsFolder </> "fireball.bmp")

    let window = InWindow "My Window" (1000, 800) (100, 100)
    let initialState = newState (playerPicture, swordPicture, fireballPicture) [enemyPicture1, enemyPicture2] gameMaps tileMap gameTiles

    play window black fps initialState render handleEvents update
  where
    loadTile tilePath = do
      tileJson <- BSL.readFile tilePath
      let tile = decode tileJson :: Maybe Tile

      case tile of
        Just t -> return t
        Nothing -> error $ "Could not load tile " ++ tilePath

    loadMap mapPath = do
      mapContent <- BSL.readFile mapPath
      let map_ = decode mapContent :: Maybe (Map Char)

      case map_ of
        Just m -> return m
        Nothing -> error $ "Could not load map " ++ mapPath
