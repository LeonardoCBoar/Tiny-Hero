{-# LANGUAGE ImportQualifiedPost #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Renderer (renderMap, renderPlayer, renderEnemies, screenPositionToWorldPosition, renderHUD, renderPossibleMoves) where

import Config (halfTileSize, scalingFactor, tileSize)
import Data.Map qualified as M
import Game (Enemy (..), Entity (ePos, eTexture), Map (..), Mode (MoveMode), Player (pEnt, pMaxMoveDistance), State (..), Tile (Tile, tWalkable), World (..), getTileFromName, isMapBounded, (!!!))
import Graphics.Gloss (Picture, Point, circle, color, pictures, scale, text, translate, white, yellow)

screenPositionToWorldPosition :: Point -> Point
screenPositionToWorldPosition (mouseX, mouseY) = (fromIntegral x, fromIntegral y)
  where
    rmx = (fromIntegral . floor) mouseX
    rmy = (fromIntegral . floor) mouseY
    x = floor $ (1 / (scalingFactor * tileSize)) * (rmx / 2 + rmy)
    y = floor $ (1 / (scalingFactor * tileSize)) * (rmy - rmx / 2)

renderHUD :: State World -> Picture
renderHUD state = pictures [renderActionsHelperText]

renderActionsHelperText :: Picture
renderActionsHelperText =
  scale 0.2 0.2 $
    pictures
      [ translate (-2000) (-800) $ color white $ text "Press 'm' to enter move mode",
        translate (-2000) (-1000) $ color white $ text "Press 'a' to enter attack mode"
      ]

renderTile :: State World -> (Float, Float, Tile) -> Picture
renderTile state (x, y, Tile _ texPath _) = translate x' y' tex
  where
    world = sData state
    worldTiles = wPictureTileMap world
    x' = (x - y) * tileSize
    y' = (x + y) * halfTileSize
    tex = worldTiles M.! texPath

renderMap :: State World -> Picture
renderMap state = pictures $ renderTile state <$> tiles
  where
    world = sData state

    zipTiles :: [[Tile]] -> [(Float, Float, Tile)]
    zipTiles = concat . zipWith (\y -> zipWith (\x -> (x,y,)) [0 ..]) [0 ..]

    tiles = reverse $ zipTiles $ mTiles $ (!! wCurrentMap world) $ wMaps world

renderPossibleMoves :: State World -> Picture
renderPossibleMoves state = case mode of
  MoveMode walkableTilesInMoveRange -> pictures $ map (renderTile state . (\(x, y) -> (x, y, indicatorTile))) walkableTilesInMoveRange
  _ -> pictures []
  where
    world = sData state
    mode = wMode world
    indicatorTile = getTileFromName "Indicator" $ wTiles world

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
