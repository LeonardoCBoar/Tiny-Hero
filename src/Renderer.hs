{-# LANGUAGE ImportQualifiedPost #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Renderer (renderMap, renderPlayer, renderEnemies, screenPositionToWorldPosition) where

import Config (halfTileSize, scalingFactor, tileSize)
import Data.Map qualified as M
import Game (Entity (ePos, eTexture), Map (..), Player (pEnt), Enemy(..), State (..), Tile (Tile), World (..), (!!!))
import Graphics.Gloss (Picture, Point, circle, color, pictures, translate, yellow)

screenPositionToWorldPosition :: Point -> Point
screenPositionToWorldPosition (mouseX, mouseY) = (fromIntegral x, fromIntegral y)
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
renderPlayer (State world _ _ _ _) = renderEntity playerEntity
  where
    playerEntity = pEnt $ wPlayer world

renderEnemies :: State World -> Picture
renderEnemies (State world _ _ _ _) = pictures [renderEntity $ eEnt enemy | enemy <- enemies]
  where 
    enemies = wEnemies world

renderEntity :: Entity -> Picture
renderEntity entity = translate x y texture
  where
    (px, py) = ePos entity
    x = (px - py) * tileSize
    y = (px + py) * halfTileSize + tileSize
    texture = eTexture entity 
