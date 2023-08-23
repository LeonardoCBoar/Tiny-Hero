{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Game (State (..), World (..), Player (..), Asset (..), AssetId (..), Map (..), Tile (..), Grid, Action(..),
  isValidAction, updatePlayer, updateWorld, isKeyPressed, insertKey, deleteKey, newState, tileFromAsset, findAssetId, getAssetFromId) where

import Data.Function (on)
import Data.Set qualified as S
import Graphics.Gloss.Interface.IO.Interact (Key, Picture, Point, Key(..), SpecialKey(..))
import System.FilePath (splitExtension)

data State a = State
  { sData :: a,
    sKeys :: S.Set Key,
    updateTimer :: Float,
    playerAction :: Action
  }

isKeyPressed :: Key -> State a -> Bool
isKeyPressed k = S.member k . sKeys

insertKey :: Key -> State a -> State a
insertKey k s = s {sKeys = S.insert k (sKeys s)}

deleteKey :: Key -> State a -> State a
deleteKey k s = s {sKeys = S.delete k (sKeys s)}

data Action = NoAction | Move Point | Attack Point
isValidAction :: Action -> Bool
isValidAction NoAction = False
isValidAction _ = True

data Player = Player
  { pPos :: Point
  }

updateWorld :: State World -> World
updateWorld state = world {wPlayer = player}
  where
    world = sData state
    player = updatePlayer (playerAction state) (wPlayer world)

updatePlayer :: Action -> Player -> Player
updatePlayer (Move (x, y)) player = player {pPos = (pX + x, pY + y)}
    where
      pX = fst $ pPos player
      pY = snd $ pPos player
updatePlayer _ player = player

data Asset = Asset
  { aName :: String,
    aPicture :: Picture
  }

instance Eq Asset where
  (==) = (==) `on` aName

instance Ord Asset where
  compare = compare `on` aName

newtype AssetId = AssetId Int

data World = World
  { wPlayer :: Player,
    wAssets :: S.Set Asset,
    wMap :: Map
  }

newState :: [Asset] -> Map -> State World
newState assets map_ =
  State
    { sData =
        World
          { wPlayer =
              Player
                { pPos = (0, 0)
                },
            wMap = map_,
            wAssets = S.fromList assets
          },
      sKeys = S.empty,
      updateTimer = 0.0,
      playerAction = NoAction
    }

findAssetId :: State World -> (Asset -> Bool) -> AssetId
findAssetId world f = findAssetId' (S.toList $ wAssets $ sData world) 0
  where
    findAssetId' [] _ = error "Asset not found"
    findAssetId' (x : xs) i
      | f x = AssetId i
      | otherwise = findAssetId' xs (i + 1)

getAssetFromId :: State World -> AssetId -> Asset
getAssetFromId world (AssetId i) = S.elemAt i $ wAssets $ sData world

type Grid = [[Tile]]

data Tile
  = Tile
      { tTexture :: AssetId,
        tSolid :: Bool,
        tPos :: Point
      }
  | SmartTile
      { tTextures :: [AssetId],
        tSolid :: Bool,
        tPos :: Point
      }

data Map = Map
  { mName :: String,
    mLayers :: [Grid]
  }

tileFromAsset :: State World -> String -> Bool -> Point -> Tile
tileFromAsset world path = Tile tilePicture
  where
    tilePicture = findAssetId world (\x -> aName x == snd (splitExtension path))
