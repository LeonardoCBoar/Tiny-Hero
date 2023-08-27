{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Config (charactersFolder, fps, halfTileSize, mapsFolder, scalingFactor, tileSize, tilesFolder)
-- REMOVER DEPOIS DE TESTAR!!!!!!!!!!!!!

import Data.Aeson hiding (Key)
import Data.ByteString.Lazy qualified as BSL
import Data.List (break, groupBy)
import Data.Map qualified as M
import Debug.Trace
import Game
  ( Action (..),
    Enemy (..),
    Entity (..),
    Map (..),
    Mode (MoveMode, NoMode),
    Player (..),
    State (State, lastMousePosition, playerAction, sData, updateTimer),
    Tile (..),
    World (World, wCurrentMap, wEnemies, wMaps, wMode, wPlayer),
    createMaps,
    deleteKey,
    insertKey,
    isMapBounded,
    isValidAction,
    newState,
    updatePlayer,
    updateWorld,
    (!!!),
  )
import Graphics.Gloss (Display (InWindow), Picture, Point, black, circle, color, loadBMP, pictures, play, scale, translate, yellow)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char, MouseButton, SpecialKey), KeyState (Down, Up), MouseButton (LeftButton), SpecialKey (..))
import Renderer (renderActionsHelperText, renderEnemies, renderMap, renderPlayer, renderPossibleMoves, screenPositionToWorldPosition)
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, splitExtension, takeFileName, (</>))

-- REMOVER DEPOIS DE TESTAR!!!!!!!!!!!!!
-- TODO: REMOVER TRACES ANTES DE ENTREGAR O PROJETO

render :: State World -> Picture
render state = pictures [scale scalingFactor scalingFactor $ pictures renderAll, renderActionsHelperText]
  where
    renderAll = map (\f -> f state) [renderMap, renderPossibleMoves, renderPlayer, renderEnemies]

getActionFromKey :: Key -> Action
getActionFromKey (SpecialKey KeyLeft) = Move (-1, 0)
getActionFromKey (SpecialKey KeyRight) = Move (1, 0)
getActionFromKey (SpecialKey KeyUp) = Move (0, -1)
getActionFromKey (SpecialKey KeyDown) = Move (0, 1)
getActionFromKey (SpecialKey KeySpace) = Move (0, 0)
getActionFromKey _ = NoAction

updateInterval :: Float
updateInterval = 0.5

handleEvents :: Event -> State World -> State World
handleEvents (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) state = case mode of
  MoveMode walkableTiles ->
    if (x, y) `elem` walkableTiles
      then state {lastMousePosition = (x, y), playerAction = Move (x - px, y - py)}
      else state {lastMousePosition = (x, y)}
  _ -> state
  where
    (x, y) = screenPositionToWorldPosition (mouseX, mouseY)
    world = sData state
    mode = wMode world
    player = wPlayer world
    playerEntity = pEnt player
    (px, py) = ePos $ playerEntity
handleEvents (EventKey key keyState _ _) state
  | updateTimer state < updateInterval = state
  | key == Char 'm' && keyState == Down =
      if isMoveMode
        then state {sData = world {wMode = NoMode}}
        else state {sData = world {wMode = MoveMode walkableTilesInMoveRange}}
  | keyState == Down && key `elem` actionKeys = state {playerAction = getActionFromKey key}
  | keyState == Down = insertKey key state
  | keyState == Up = deleteKey key state
  | otherwise = state
  where
    world = sData state
    isMoveMode = case wMode world of
      MoveMode _ -> True
      _ -> False

    actionKeys = [SpecialKey KeySpace, SpecialKey KeyUp, SpecialKey KeyDown, SpecialKey KeyLeft, SpecialKey KeyRight]
    currentMap = (!! wCurrentMap world) $ wMaps world
    player = wPlayer world
    (px, py) = ePos $ pEnt player
    maxMoveDistance = fromIntegral $ pMaxMoveDistance player
    tilesInMoveRange =
      [ (x, y)
        | x <- [px - maxMoveDistance .. px + maxMoveDistance],
          y <- [py - maxMoveDistance .. py + maxMoveDistance],
          abs (x - px) + abs (y - py) <= maxMoveDistance
      ]
    walkableTilesInMoveRange =
      filter
        ( \tilePos ->
            isMapBounded currentMap tilePos
              && tWalkable (currentMap !!! tilePos)
        )
        tilesInMoveRange
handleEvents _ state = state

{-
x' = (x - y) * 2s
x' = 2s * x - 2s * y
2s * y = 2s * x - x'
y = x - x' / 2s
y = (y' - x' / 2) / 2s
y = (y' - x') * s

y' = (x + y) * s
y' = (x + x - x' / 2s) * s
y' = 2sx - x' / 2
2sx = y' + x' / 2
x = (y' + x' / 2) / 2s
x = (y' + x') * s

-}

update :: Float -> State World -> State World
update dt state
  | isValidAction action = do
      trace
        ( "Player: "
            ++ show (ePos $ pEnt $ wPlayer $ sData state)
            ++ "Enemy: "
            ++ show (ePos $ eEnt $ head $ wEnemies $ sData state)
        )
        state {sData = updateWorld state, updateTimer = 0, playerAction = NoAction}
  | otherwise = state {updateTimer = curUpdateTimer}
  where
    action = playerAction state
    curUpdateTimer = updateTimer state + dt

hasExt :: FilePath -> String -> Bool
hasExt path ext = snd (splitExtension path) == ext

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
    meleeEnemyPicture <- loadBMP (charactersFolder </> "ogre.bmp")

    let window = InWindow "My Window" (640, 480) (100, 100)
    let initialState = newState (playerPicture, meleeEnemyPicture) gameMaps tileMap gameTiles

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
