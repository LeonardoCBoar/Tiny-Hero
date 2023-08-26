{-# LANGUAGE ImportQualifiedPost #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Renderer (renderMap) where

import Config (halfTileSize, tileSize)
import Data.Map qualified as M
import Game (Map (..), State (..), Tile (Tile), World (..), (!!!))
import Graphics.Gloss (Picture, circle, color, pictures, translate, yellow)

renderMap :: State World -> Picture
renderMap state = pictures $ renderTile <$> tiles
  where
    world = sData state
    worldTiles = wPictureTileMap world

    zipTiles :: [[Tile]] -> [(Float, Float, Tile)]
    zipTiles = concat . zipWith (\y -> zipWith (\x -> (x,y,)) [0 ..]) [0 ..]

    tiles = reverse $ zipTiles $ mTiles $ (!! wCurrentMap world) $ wMaps world

    renderTile :: (Float, Float, Tile) -> Picture
    renderTile (x, y, Tile _ texPath _) = translate x' y' tex
      where
        x' = (x - y) * tileSize
        y' = (x + y) * halfTileSize
        tex = worldTiles M.! texPath
