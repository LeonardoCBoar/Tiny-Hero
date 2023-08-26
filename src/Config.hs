{-# LANGUAGE OverloadedStrings #-}

module Config (fps, assetsFolder, tilesFolder, tileImagesFolder, mapsFolder, scalingFactor, tileSize, halfTileSize) where

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

tilesFolder :: FilePath
tilesFolder = assetsFolder </> "tiles"

imagesFolder :: FilePath
imagesFolder = assetsFolder </> "images"

tileImagesFolder :: FilePath
tileImagesFolder = imagesFolder </> "tiles"

mapsFolder :: FilePath
mapsFolder = assetsFolder </> "maps"
