{-# OPTIONS_GHC -Wno-partial-fields #-}

module Tile (Tile (..)) where

import Graphics.Gloss (Picture)

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
