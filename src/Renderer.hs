module Renderer (renderMap) where

import Config (tileSize)
import Game (Map (..), State (..), World (..))
import Graphics.Gloss (Picture, circle, color, pictures, translate, yellow)
import Tile (Tile (..))

zipPosition :: [[Tile]] -> [(Int, Int, Tile)]
zipPosition grid = do
  (y, line) <- zip [0 ..] grid
  (x, tile) <- zip [0 ..] line
  return (x, y, tile)

renderMap :: State World -> Picture
renderMap world = pictures $ map renderTile posTiles
  where
    map_ = wMap $ sData world
    posTiles = zipPosition $ mTiles map_

    renderTile :: (Int, Int, Tile) -> Picture
    renderTile (x, y, Tile {tTexture = texture}) = translate (tileSize * fromIntegral x) (tileSize * fromIntegral y) texture
    renderTile (x, y, EmptyTile) = translate (tileSize * fromIntegral x) (tileSize * fromIntegral y) (color yellow $ circle 1)
    -- TODO: render smart tiles correctly
    renderTile (x, y, SmartTile {tTextures = textures}) = translate (tileSize * fromIntegral x) (tileSize * fromIntegral y) (head textures)
