{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Config
import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as M
import Events (handleGameEvents, handleGameOverEvents, handleMenuEvents, handleWinEvents)
import Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Renderer
  ( renderAttackAnimation,
    renderEnemies,
    renderGameOver,
    renderHUD,
    renderMainMenu,
    renderMap,
    renderPlayer,
    renderPossibleMoves,
    renderWin,
  )
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import Update

render :: State World -> Picture
render state
  | scene == Menu = renderMainMenu state
  | scene == Game = pictures [scale scalingFactor scalingFactor $ pictures renderAll, renderHUD state]
  | scene == GameOver = renderGameOver state
  | scene == Win = renderWin state
  | otherwise = blank
  where
    scene = currentScene state
    renderAll =
      map
        (\f -> f state)
        [ renderMap,
          renderPossibleMoves,
          renderPlayer,
          renderEnemies,
          renderAttackAnimation
        ]

handleEvents :: Event -> State World -> State World
handleEvents event state
  | scene == Menu = handleMenuEvents event state
  | scene == Game = handleGameEvents event state
  | scene == GameOver = handleGameOverEvents event state
  | scene == Win = handleWinEvents event state
  | otherwise = state
  where
    scene = currentScene state

update :: Float -> State World -> State World
update dt state
  | scene == Game = updateGame dt state
  | otherwise = state
  where
    scene = currentScene state

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

    let window = InWindow "My Window" (screenWidth, screenHeight) (100, 100)
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
