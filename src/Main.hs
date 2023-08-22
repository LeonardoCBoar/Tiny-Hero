module Main (main) where

import Config (fps)
import Game (Asset (Asset), AssetId (AssetId), Map (..), Player (pPos), State (State, sData), Tile (..), World (World, wPlayer), deleteKey, insertKey, newState)
import Graphics.Gloss (Display (InWindow), Picture, black, circle, color, loadBMP, play, translate, yellow)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), KeyState (Down, Up))
import Renderer (renderMap)
import System.Directory (getDirectoryContents)
import System.FilePath (splitExtension, (</>))

render :: State World -> Picture
render = renderMap

handleEvents :: Event -> State World -> State World
handleEvents (EventKey key keyState _ _) state
  | keyState == Down = insertKey key state
  | keyState == Up = deleteKey key state
  | otherwise = state
handleEvents _ state = state

update :: Float -> State World -> State World
update dt state = state

main :: IO ()
main =
  do
    imageAssets <- getDirectoryContents "assets"

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
    loadAsset assetPath = do
      assetPicture <- loadBMP ("assets" </> assetPath)
      return $ Asset assetName assetPicture
      where
        assetName = fst $ splitExtension assetPath
