{-# LANGUAGE ImportQualifiedPost #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Renderer
  ( renderMap,
    renderPlayer,
    renderEnemies,
    screenPositionToWorldPosition,
    renderHUD,
    renderPossibleMoves,
    renderGameModeText,
    renderAttackAnimation,
    renderMainMenu,
    renderGameOver,
    renderWin,
  )
where

import Config (halfTileSize, scalingFactor, tileSize)
import Data.Map qualified as M
import Game
import Graphics.Gloss

screenPositionToWorldPosition :: Point -> Point
screenPositionToWorldPosition (mouseX, mouseY) = (fromIntegral x, fromIntegral y)
  where
    rmx = (fromIntegral . floor) mouseX
    rmy = (fromIntegral . floor) mouseY
    x = floor $ (1 / (scalingFactor * tileSize)) * (rmx / 2 + rmy)
    y = floor $ (1 / (scalingFactor * tileSize)) * (rmy - rmx / 2)

worldPositionToScreenPosition :: Point -> Point
worldPositionToScreenPosition (x, y) = (x', y')
  where
    x' = (x - y) * tileSize
    y' = (x + y) * halfTileSize + tileSize

renderHUD :: State World -> Picture
renderHUD state = pictures $ map (scale 0.2 0.2) [renderActionsHelperText, renderGameModeText state, renderLifeBar player]
  where
    world = sData state
    player = wPlayer world

renderLifeBar :: Player -> Picture
renderLifeBar player = pictures [background, foreground]
  where
    background = color white $ translate (-2150) 1900 $ rectangleSolid 510 110
    foreground = color red $ translate (-2150) 1900 $ rectangleSolid (500 * playerLifeRatio) 100
    playerStats = eStats $ pEnt player
    playerLifeRatio = fromIntegral (life playerStats) / fromIntegral (maxLife playerStats)

renderGameModeText :: State World -> Picture
renderGameModeText state = translate (-2000) (-600) $ color white $ text $ case mode of
  MoveMode _ -> "Move mode"
  AttackMode _ -> "Attack mode"
  _ -> "Waiting for mode selection..."
  where
    world = sData state
    mode = wMode world

renderActionsHelperText :: Picture
renderActionsHelperText =
  pictures
    [ translate (-2000) (-800) $ color white $ text "Press m to enter move mode",
      translate (-2000) (-1000) $ color white $ text "Press a to enter attack mode"
    ]

renderTile :: State World -> (Float, Float, Tile) -> Picture
renderTile state (x, y, Tile _ texPath _) = translate x' y' tex
  where
    world = sData state
    worldTiles = wPictureTileMap world
    x' = (x - y) * tileSize
    y' = (x + y) * halfTileSize
    tex = worldTiles M.! texPath
renderTile _ (_, _, EmptyTile) = pictures []

renderMap :: State World -> Picture
renderMap state
  | wCurrentMap world >= length (wMaps world) = blank
  | otherwise = pictures $ renderTile state <$> tiles
  where
    world = sData state

    zipTiles :: [[Tile]] -> [(Float, Float, Tile)]
    zipTiles = concat . zipWith (\y -> zipWith (\x -> (x,y,)) [0 ..]) [0 ..]

    tiles = reverse $ zipTiles $ mTiles $ (!! wCurrentMap world) $ wMaps world

renderPossibleMoves :: State World -> Picture
renderPossibleMoves state = case mode of
  MoveMode walkableTilesInMoveRange -> renderMove indicatorTile walkableTilesInMoveRange
  AttackMode attackableTilesInAttackRange -> renderMove attackIndicatorTile attackableTilesInAttackRange
  _ -> pictures []
  where
    world = sData state
    mode = wMode world
    indicatorTile = getTileFromName "Indicator" $ wTiles world
    attackIndicatorTile = getTileFromName "AttackIndicator" $ wTiles world
    renderMove tile tileList = pictures $ map (renderTile state . (\(x, y) -> (x, y, tile))) tileList

renderAttackAnimation :: State World -> Picture
renderAttackAnimation state
  | showAttackAnimation state = translate x y $ rotate (timer * 50) sword
  | otherwise = pictures []
  where
    (x, y) = worldPositionToScreenPosition $ lastMousePosition state
    timer = updateTimer state
    world = sData state
    sword = wSword world

renderPlayer :: State World -> Picture
renderPlayer state = renderEntity playerEntity
  where
    world = sData state
    playerEntity = pEnt $ wPlayer world

renderEnemies :: State World -> Picture
renderEnemies state = pictures [attackIndicator, pictures [renderEntity $ eEnt enemy | enemy <- mapEnemies]]
  where
    world = sData state
    mapEnemies = wEnemies world

    rangedEnemies = filter isRanged mapEnemies
    rangedEnemiesInAttackDistance =
      filter
        (\enemy -> manhattanDist (pointDiff (ePos (eEnt enemy)) (ePos $ pEnt $ wPlayer world)) <= 8)
        rangedEnemies

    attackIndicator =
      pictures $
        map
          (\enemy -> let (x, y) = worldPositionToScreenPosition $ ePos $ eEnt enemy in translate x (y + 20) $ wFireball world)
          rangedEnemiesInAttackDistance

    isRanged (Melee _ _) = False
    isRanged _ = True

renderEntity :: Entity -> Picture
renderEntity entity = translate x y texture
  where
    (px, py) = ePos entity
    x = (px - py) * tileSize
    y = (px + py) * halfTileSize + tileSize
    texture = eTexture entity

renderMainMenu :: State World -> Picture
renderMainMenu _ = pictures [title, message]
  where
    title = scale 0.5 0.5 $ translate (-300) 0 $ color white $ text "Tiny Hero"
    message = scale 0.15 0.15 $ translate (-600) (-400) $ color white $ text "Press space to start"

renderGameOver :: State World -> Picture
renderGameOver _ = pictures [title, message]
  where
    title = scale 0.5 0.5 $ translate (-300) 0 $ color white $ text "Game Over"
    message = scale 0.15 0.15 $ translate (-600) (-400) $ color white $ text "Press space to restart"

renderWin :: State World -> Picture
renderWin _ = pictures [title, message]
  where
    title = scale 0.5 0.5 $ translate (-300) 0 $ color white $ text "You Won!"
    message = scale 0.15 0.15 $ translate (-600) (-400) $ color white $ text "Press space to restart"
