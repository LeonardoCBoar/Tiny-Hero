{-# LANGUAGE OverloadedStrings #-}

module Config (fps, assetsFolder, tilesFolder, solidTilesFolder, nonSolidTilesFolder, mapsFolder, scalingFactor, tileSize) where

import System.FilePath ((</>))

fps :: Int
fps = 60

scalingFactor :: Float
scalingFactor = 4

tileSize :: Float
tileSize = 16

assetsFolder :: FilePath
assetsFolder = "assets"

imagesFolder :: FilePath
imagesFolder = assetsFolder </> "images"

tilesFolder :: FilePath
tilesFolder = imagesFolder </> "tiles"

solidTilesFolder :: FilePath
solidTilesFolder = tilesFolder </> "solid"

nonSolidTilesFolder :: FilePath
nonSolidTilesFolder = tilesFolder </> "non-solid"

mapsFolder :: FilePath
mapsFolder = assetsFolder </> "maps"
