{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Config (fps, mapsFolder, nonSolidTilesFolder, scalingFactor, solidTilesFolder, tilesFolder)
-- REMOVER DEPOIS DE TESTAR!!!!!!!!!!!!!

import Data.Aeson hiding (Key)
import Data.ByteString.Lazy qualified as BSL
import Data.List (break, groupBy)
import Data.Map qualified as M
import Debug.Trace
import Game
  ( Action (..),
    Map (..),
    Entity(..),
    Player (..),
    Enemy (..),
    State (State, playerAction, sData, updateTimer),
    World (World, wPlayer, wEnemies),
    deleteKey,
    insertKey,
    isValidAction,
    newState,
    tilesFromChar,
    updatePlayer,
    updateWorld,
    withMap,
  )
import Graphics.Gloss (Display (InWindow), Picture, black, circle, color, loadBMP, play, scale, translate, yellow)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (SpecialKey), KeyState (Down, Up), SpecialKey (..))
import Renderer (renderMap)
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, splitExtension, takeFileName, (</>))
import Tile (Tile (EmptyTile, SmartTile, Tile))

-- REMOVER DEPOIS DE TESTAR!!!!!!!!!!!!!
-- TODO: REMOVER TRACES ANTES DE ENTREGAR O PROJETO

render :: State World -> Picture
render state = scale scalingFactor scalingFactor $ renderMap state

getActionFromKey :: Key -> Action
getActionFromKey (SpecialKey KeyLeft)  = Move (-1, 0)
getActionFromKey (SpecialKey KeyRight) = Move (1, 0)
getActionFromKey (SpecialKey KeyUp)    = Move (0,-1)
getActionFromKey (SpecialKey KeyDown)  = Move (0, 1)
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
        ("Player: " ++ show ( ePos $ pEnt $ wPlayer $ sData state) ++
         "Enemy: " ++ show  ( ePos $ eEnt $ head $ wEnemies $ sData state))
        state {sData = updateWorld state, updateTimer = 0, playerAction = NoAction}
  | otherwise = state {updateTimer = curUpdateTimer}
  where
    action = playerAction state
    curUpdateTimer = updateTimer state + dt

isBmpFile :: FilePath -> Bool
isBmpFile path = snd (splitExtension path) == ".bmp"

getTileName :: [Char] -> [Char]
getTileName = takeWhile (/= '_')

groupTiles :: [FilePath] -> [[FilePath]]
groupTiles ts = groupTiles' ts M.empty
  where
    groupTiles' :: [FilePath] -> M.Map String [FilePath] -> [[FilePath]]
    groupTiles' [] acc = M.elems acc
    groupTiles' (x : xs) acc = groupTiles' xs (M.insertWith (++) (getTileName x) [x] acc)

main :: IO ()
main =
  do
    solidTilesContent <- getDirectoryContents solidTilesFolder
    nonSolidTilesContent <- getDirectoryContents nonSolidTilesFolder

    let solidTiles = map (solidTilesFolder </>) $ filter isBmpFile solidTilesContent
    let solidTiles' = groupTiles solidTiles

    let nonSolidTiles = map (nonSolidTilesFolder </>) $ filter isBmpFile nonSolidTilesContent
    let nonSolidTiles' = groupTiles nonSolidTiles

    solidTiles'' <- mapM (mapM loadTexture) solidTiles'
    nonSolidTiles'' <- mapM (mapM loadTexture) nonSolidTiles'

    let solidTiles''' = map (`createTleOrSmartTile` True) solidTiles''
    let nonSolidTiles''' = map (`createTleOrSmartTile` False) nonSolidTiles''

    let tiles = EmptyTile : (solidTiles''' ++ nonSolidTiles''')
    let window = InWindow "My Window" (640, 480) (100, 100)

    mapContent <- BSL.readFile (mapsFolder </> "test.json")
    let charsMap = case decode mapContent :: Maybe (Map Char) of
          Just map' -> map'
          Nothing -> error "Error parsing map"

    let initialState = newState tiles
    let tilesMap = tilesFromChar initialState <$> charsMap
    let state = tilesMap `withMap` initialState

    play window black fps state render handleEvents update
  where
    loadTexture :: FilePath -> IO (String, Picture)
    loadTexture path = do
      texture <- loadBMP path

      let name = takeFileName path
      return (name, texture)

    createTile :: (String, Picture) -> Bool -> Tile
    createTile (name, picture) = Tile (takeWhile (/= '_') (dropExtension name)) picture

    createSmartTile :: [(String, Picture)] -> Bool -> Tile
    createSmartTile ((name, p) : ps) = SmartTile (takeWhile (/= '_') (dropExtension name)) (p : map snd ps)

    createTleOrSmartTile :: [(String, Picture)] -> Bool -> Tile
    createTleOrSmartTile [x] = createTile x
    createTleOrSmartTile xs = createSmartTile xs
