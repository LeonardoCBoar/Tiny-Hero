{-# OPTIONS_GHC -Wno-partial-fields #-}

module Tile (Tile (..), tilePositionToTextureIndex) where

import Graphics.Gloss (Picture, Point)

data Tile
  = Tile
      { tName :: String,
        tTexture :: Picture,
        tIsSolid :: Bool
      }
  | SmartTile
      { tName :: String,
        tTextures :: [Picture],
        tIsSolid :: Bool
      }
  | EmptyTile
  deriving (Show)

tilePositionToTextureIndex :: Point -> Int
tilePositionToTextureIndex (x, y) = case (x, y) of
  (0, 0) -> 4
  (0, 1) -> 1
  (0, -1) -> 7
  (1, 0) -> 3
  (1, 1) -> 0
  (1, -1) -> 6
  (-1, 0) -> 5
  (-1, 1) -> 2
  (-1, -1) -> 8
  _ -> 4
