{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Game
  ( State (..),
    World (..),
    Player (..),
    Action (..),
    Map (..),
    isValidAction,
    updatePlayer,
    updateWorld,
    isKeyPressed,
    insertKey,
    deleteKey,
    newState,
    tilesFromChar,
    withMap,
  )
where

import Data.Aeson hiding (Key)
import Data.Set qualified as S
import Debug.Trace (trace)
import GHC.Generics
import Graphics.Gloss.Interface.IO.Interact (Key (..), Picture, Point, SpecialKey (..))
import Tile (Tile (..))

data State a = State
  { sData :: a,
    sKeys :: S.Set Key,
    updateTimer :: Float,
    playerAction :: Action
  }
  deriving (Show)

isKeyPressed :: Key -> State a -> Bool
isKeyPressed k = S.member k . sKeys

insertKey :: Key -> State a -> State a
insertKey k s = s {sKeys = S.insert k (sKeys s)}

deleteKey :: Key -> State a -> State a
deleteKey k s = s {sKeys = S.delete k (sKeys s)}

data Action = NoAction | Move Point | Attack Point deriving (Show)

isValidAction :: Action -> Bool
isValidAction NoAction = False
isValidAction _ = True

data Player = Player
  { pPos :: Point
  }
  deriving (Show)

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

-- newtype JsonMap = JsonMap
--   { jTiles :: [[Char]]
--   }
--   deriving (Show, Generic)

-- instance ToJSON JsonMap where
--   toEncoding = genericToEncoding defaultOptions

-- instance FromJSON JsonMap where
--   parseJSON = withObject "JsonMap" $ \o -> do
--     jsonTiles <- o .: "tiles"
--     return JsonMap {jTiles = jsonTiles}

newtype Map a = Map
  { mTiles :: [[a]]
  }
  deriving (Show, Generic)

instance (ToJSON a) => ToJSON (Map a) where
  toEncoding = genericToEncoding defaultOptions

instance (FromJSON a) => FromJSON (Map a) where
  parseJSON = withObject "Map" $ \o -> do
    jsonTiles <- o .: "tiles"
    return Map {mTiles = jsonTiles}

instance Functor Map where
  fmap f Map {mTiles = tiles} = Map {mTiles = map (map f) tiles}

instance Applicative Map where
  pure a = Map [[a]]
  Map [[f]] <*> Map values = Map $ map (map f) values
  Map fs <*> Map values = Map $ zipWith (zipWith ($)) fs values

data World = World
  { wPlayer :: Player,
    wTiles :: [Tile],
    wMap :: Map Tile
  }
  deriving (Show)

findTile :: String -> [Tile] -> Tile
findTile name tiles =
  head $
    filter
      ( \t -> case t of
          EmptyTile -> False
          _ -> tName t == name
      )
      tiles

newState :: [Tile] -> State World
newState tiles =
  State
    { sData =
        World
          { wPlayer =
              Player
                { pPos = (0, 0)
                },
            wTiles = tiles,
            wMap = Map {mTiles = []}
          },
      sKeys = S.empty,
      updateTimer = 0.0,
      playerAction = NoAction
    }

withMap :: Map Tile -> State World -> State World
withMap map' state = state {sData = world {wMap = map'}}
  where
    world = sData state

tilesFromChar :: State World -> Char -> Tile
tilesFromChar state c
  | c == ' ' = EmptyTile
  | otherwise = findTile name tiles
  where
    tiles = wTiles $ sData state
    name = case c of
      'N' -> "dirt"
      'W' -> "wall"
      'F' -> "sand"
      _ -> "error: " ++ [c]
