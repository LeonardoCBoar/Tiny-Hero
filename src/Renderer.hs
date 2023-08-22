module Renderer (renderMap) where

import Game (Asset (Asset), Grid, Map (..), State (..), Tile (..), World (..), getAssetFromId)
import Graphics.Gloss (Picture, pictures, translate)

renderLayer :: State World -> Grid -> Picture
renderLayer state grid = pictures lines'
  where
    lines' = map renderLine grid

    -- TODO: handle smart tiles
    renderTile :: Tile -> Picture
    renderTile tile = translate x y assetPicture
      where
        (x, y) = tPos tile
        assetId = tTexture tile
        Asset _ assetPicture = getAssetFromId state assetId

    renderLine :: [Tile] -> Picture
    renderLine tiles = pictures $ map renderTile tiles

renderMap :: State World -> Picture
renderMap world = pictures layers
  where
    map_ = wMap $ sData world
    layers = map (renderLayer world) $ mLayers map_
