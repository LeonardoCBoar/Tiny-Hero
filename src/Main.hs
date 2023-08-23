module Main (main) where

import Config (fps)
import Game (Asset (Asset), AssetId (AssetId), Map (..), Player (pPos), updatePlayer, State (State, sData, updateTimer, playerAction),
  Action(..), Tile (..), World (World, wPlayer), updateWorld, deleteKey, insertKey, newState, isValidAction, updateWorld)
import Graphics.Gloss (Display (InWindow), Picture, black, circle, color, loadBMP, play, translate, yellow)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), KeyState (Down, Up), Key (SpecialKey), SpecialKey(..))
import Renderer (renderMap)
import System.Directory (getDirectoryContents)
import System.FilePath (splitExtension, (</>))

--REMOVER DEPOIS DE TESTAR!!!!!!!!!!!!!
import Debug.Trace
--REMOVER DEPOIS DE TESTAR!!!!!!!!!!!!!

render :: State World -> Picture
render = renderMap

getActionFromKey :: Key -> Action
getActionFromKey (SpecialKey KeyLeft)  = Move (-1, 0)
getActionFromKey (SpecialKey KeyRight) = Move ( 1, 0)
getActionFromKey (SpecialKey KeyUp)    = Move (0 ,-1)
getActionFromKey (SpecialKey KeyDown)  = Move (0 , 1)
getActionFromKey _ = NoAction

updateInterval :: Float
updateInterval = 0.5

handleEvents :: Event -> State World -> State World
handleEvents (EventKey key keyState _ _) state
  | updateTimer state < updateInterval = do trace "ignored action" state
  | keyState == Down && key `elem` actionKeys = state {playerAction = getActionFromKey key}
  | keyState == Down = insertKey key state
  | keyState == Up = deleteKey key state
  | otherwise = state
  where 
    actionKeys = [SpecialKey KeyUp, SpecialKey KeyDown, SpecialKey KeyLeft, SpecialKey KeyRight]
handleEvents _ state = state

update :: Float -> State World -> State World
update dt state
  | isValidAction action = do trace (show $ pPos $ wPlayer $ sData state) 
                                state {sData = updateWorld state, updateTimer = 0, playerAction = NoAction}
  | otherwise =  state{updateTimer = curUpdateTimer}
    where 
      action = playerAction state
      curUpdateTimer = updateTimer state + dt

mapNameToPath :: String -> String
mapNameToPath mapName = "assets" </> "maps" </> (mapName ++ "_layer0" ++ ".bmp")

main :: IO ()
main =
  do
    imageAssets <- getDirectoryContents ("assets" </> "images")

    let imageAssets' = filter (\x -> x /= "." && x /= "..") imageAssets
    assets <- mapM loadAsset imageAssets'

    let window = InWindow "My Window" (640, 480) (100, 100)
    let map_ =
          Map
            { mName = "test",
              mLayers =
                [ [ [ Tile
                        { tTexture = AssetId 6,
                          tSolid = False,
                          tPos = (0, 0)
                        },
                      Tile
                        { tTexture = AssetId 6,
                          tSolid = False,
                          tPos = (1, 0)
                        }
                    ]
                  ]
                ]
            }
    let initialState = newState assets map_

    play window black fps initialState render handleEvents update
  where
    -- loadMap mapName = do
    --   mapPicture <- loadBMP ("assets" </> "maps" </> mapNameToPath mapName)
    --   return $ Map mapName undefined

    loadAsset assetPath = do
      assetPicture <- loadBMP ("assets" </> "images" </> assetPath)
      return $ Asset assetName assetPicture
      where
        assetName = fst $ splitExtension assetPath
