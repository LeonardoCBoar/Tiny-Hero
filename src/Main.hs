{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Config (fps, halfTileSize, mapsFolder, scalingFactor, tileSize, tilesFolder)
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
    Player (..),
    State (State, playerAction, sData, updateTimer),
    Tile (..),
    World (World, wEnemies, wPlayer),
    deleteKey,
    insertKey,
    isValidAction,
    newState,
    updatePlayer,
    updateWorld,
  )
import Graphics.Gloss (Display (InWindow), Picture, black, circle, color, loadBMP, pictures, play, scale, translate, yellow)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (SpecialKey), KeyState (Down, Up), SpecialKey (..))
import Renderer (renderMap)
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, splitExtension, takeFileName, (</>))

-- REMOVER DEPOIS DE TESTAR!!!!!!!!!!!!!
-- TODO: REMOVER TRACES ANTES DE ENTREGAR O PROJETO

render :: State World -> Picture
render state = scale scalingFactor scalingFactor $ renderMap state

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
handleEvents (EventKey key keyState _ _) state
  | updateTimer state < updateInterval = state
  | keyState == Down && key `elem` actionKeys = state {playerAction = getActionFromKey key}
  | keyState == Down = insertKey key state
  | keyState == Up = deleteKey key state
  | otherwise = state
  where
    actionKeys = [SpecialKey KeySpace, SpecialKey KeyUp, SpecialKey KeyDown, SpecialKey KeyLeft, SpecialKey KeyRight]
handleEvents _ state = state

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

isBmpFile :: FilePath -> Bool
isBmpFile path = snd (splitExtension path) == ".bmp"

main :: IO ()
main =
  do
    tilesContent <- getDirectoryContents tilesFolder
    let tiles = map (tilesFolder </>) $ filter isBmpFile tilesContent

    loadedTiles <- mapM loadBMP tiles
    let gameTiles = map Tile loadedTiles

    let window = InWindow "My Window" (640, 480) (100, 100)

    -- let positions = reverse [(0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (0, 2), (1, 2), (2, 2)]
    let initialState = newState gameTiles

    play window black fps initialState render handleEvents update
