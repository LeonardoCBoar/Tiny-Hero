{-# LANGUAGE OverloadedStrings #-}

module Config (fps, assetsFolder, tilesFolder, mapsFolder, scalingFactor, tileSize, halfTileSize) where

import System.FilePath ((</>))

fps :: Int
fps = 60

scalingFactor :: Float
scalingFactor = 4

tileSize :: Float
tileSize = 16

halfTileSize :: Float
halfTileSize = tileSize / 2

assetsFolder :: FilePath
assetsFolder = "assets"

imagesFolder :: FilePath
imagesFolder = assetsFolder </> "images"

tilesFolder :: FilePath
tilesFolder = imagesFolder </> "tiles"

mapsFolder :: FilePath
mapsFolder = assetsFolder </> "maps"
