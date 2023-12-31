{-# LANGUAGE OverloadedStrings #-}

module Config
  ( fps,
    assetsFolder,
    tilesFolder,
    tileImagesFolder,
    mapsFolder,
    scalingFactor,
    tileSize,
    halfTileSize,
    charactersFolder,
    itemsFolder,
    objectsFolder,
    updateInterval,
    screenWidth,
    screenHeight,
  )
where

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

charactersFolder :: FilePath
charactersFolder = imagesFolder </> "characters"

itemsFolder :: FilePath
itemsFolder = imagesFolder </> "items"

objectsFolder :: FilePath
objectsFolder = imagesFolder </> "objects"

updateInterval :: Float
updateInterval = 0.5

screenWidth, screenHeight :: Int
(screenWidth, screenHeight) = (1200, 1000)
