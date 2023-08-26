{-# LANGUAGE ImportQualifiedPost #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Renderer (renderMap, renderPlayer, screenPositionToWorldPosition) where

import Config (halfTileSize, scalingFactor, tileSize)
import Data.Map qualified as M
import Game (Entity (ePos, eTexture), Map (..), Player (pEnt), State (..), Tile (Tile), World (..), (!!!))
import Graphics.Gloss (Picture, Point, circle, color, pictures, translate, yellow)

screenPositionToWorldPosition :: State World -> Point -> State World
screenPositionToWorldPosition state (mouseX, mouseY) = state {lastMousePosition = (fromIntegral x, fromIntegral y)}
  where
    rmx = (fromIntegral . floor) mouseX
    rmy = (fromIntegral . floor) mouseY
    x = floor $ (1 / (scalingFactor * tileSize)) * (rmx / 2 + rmy)
    y = floor $ (1 / (scalingFactor * tileSize)) * (rmy - rmx / 2)

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

renderPlayer :: State World -> Picture
renderPlayer state = translate x y texture
  where
    playerEntity = pEnt $ wPlayer $ sData state
    (px, py) = ePos playerEntity
    x = (px - py) * tileSize
    y = (px + py) * halfTileSize + tileSize
    texture = eTexture playerEntity
