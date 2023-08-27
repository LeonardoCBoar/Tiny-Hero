{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Game
  ( State (..),
    World (..),
    Player (..),
    Enemy (..),
    Entity (..),
    Stats (..),
    Action (..),
    Map (..),
    Tile (..),
    Mode (..),
    isValidAction,
    updatePlayer,
    updateWorld,
    isKeyPressed,
    insertKey,
    deleteKey,
    newState,
    isMapBounded,
    (!!!),
    createMaps,
    getTileFromName,
    isTileWalkable,
  )
where

import Data.Aeson hiding (Key)
import Data.Map qualified as M
import Data.Set qualified as S
import GHC.Generics
import Graphics.Gloss.Interface.IO.Interact (Key (..), Picture, Point)

data State a = State
  { sData :: a,
    sKeys :: S.Set Key,
    updateTimer :: Float,
    playerAction :: Action,
    lastMousePosition :: Point
  }
  deriving (Show)

isKeyPressed :: Key -> State a -> Bool
isKeyPressed k = S.member k . sKeys

insertKey :: Key -> State a -> State a
insertKey k s = s {sKeys = S.insert k (sKeys s)}

deleteKey :: Key -> State a -> State a
deleteKey k s = s {sKeys = S.delete k (sKeys s)}

data Action = NoAction | Move Point | Attack Point deriving (Show, Eq)

isValidAction :: Action -> Bool
isValidAction NoAction = False
isValidAction _ = True

data Stats = Stats
  { maxLife :: Integer,
    life :: Integer,
    attack :: Integer
  }
  deriving (Show)

data Entity = Entity
  { ePos :: Point,
    eId :: Integer, -- TODO: Implement unique IDs
    eStats :: Stats,
    eTexture :: Picture
  }
  deriving (Show)

pointDiff :: Point -> Point -> Point
pointDiff (originX, originY) (targetX, targetY) = (targetX - originX, targetY - originY)

manhattanDist :: Point -> Integer
manhattanDist (x, y) = abs (round x) + abs (round y)

newEntity :: Point -> Integer -> Integer -> Picture -> Entity
newEntity startPos startLife attack = Entity startPos 0 (Stats startLife startLife attack)

moveEntity :: Entity -> Point -> Entity
moveEntity entity (dx, dy) = entity {ePos = (x + dx, y + dy)}
  where
    pos = ePos entity
    x = fst pos
    y = snd pos

attackEntity :: Entity -> Integer -> Entity
attackEntity entity attack = entity {eStats = entStats {life = oldLife - attack}}
  where
    entStats = eStats entity
    oldLife = life entStats

moveEntityTowards :: Entity -> Point -> Entity -- TODO: Check collisions
moveEntityTowards entity (distanceX, distanceY)
  | abs distanceX >= abs distanceY = moveEntity entity (distanceX / abs distanceX, 0)
  | otherwise = moveEntity entity (0, distanceY / abs distanceY)

data Player = Player {pEnt :: Entity, pMaxMoveDistance :: Int}
  deriving (Show)

updatePlayer :: Action -> Player -> Integer -> Player
updatePlayer (Move dir) (Player ent moveDistance) damage = Player (attackEntity (moveEntity ent dir) damage) moveDistance
updatePlayer _ (Player ent moveDistance) damage = Player (attackEntity ent damage) moveDistance

data EnemyState = EIdle | EFollow | EAttack deriving (Show, Eq)

data Enemy
  = Melee {eEnt :: Entity, eState :: EnemyState}
  | Ranged {eEnt :: Entity, eState :: EnemyState}
  deriving (Show)

updateEnemy :: State World -> Enemy -> Enemy
updateEnemy (State world _ _ _ _) enemy
  | playerDist == 0 = error "Impossible collision"
  | playerDist <= 1 = enemy {eState = EAttack}
  | otherwise = Melee (moveEntityTowards (eEnt enemy) playerDir) EFollow -- TODO: Support ranged
  where
    playerDist = manhattanDist playerDir
    playerDir = pointDiff enemyPos playerPos
    enemyPos = ePos $ eEnt enemy
    playerPos = ePos $ pEnt player
    player = wPlayer world

sumEnemiesAttack :: [Enemy] -> Integer
sumEnemiesAttack enemies = sum [attack $ eStats $ eEnt enemy | enemy <- enemies, eState enemy == EAttack]

updateWorld :: State World -> World
updateWorld state = world {wPlayer = player, wEnemies = enemies, wMode = mode}
  where
    world = sData state
    pAction = playerAction state
    mode =
      if pAction == NoAction
        then NoMode
        else EnemyMode
    enemies = map (updateEnemy state) (wEnemies world)
    enemiesAttack = sumEnemiesAttack enemies
    player = updatePlayer pAction (wPlayer world) enemiesAttack

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

data Tile
  = Tile
      { tName :: String,
        tTexture :: String,
        tWalkable :: Bool
      }
  | EmptyTile
  deriving (Show, Generic)

isTileWalkable :: Tile -> Bool
isTileWalkable (Tile _ _ walkable) = walkable
isTileWalkable EmptyTile = False

instance ToJSON Tile where
  toEncoding (Tile name texture walkable) = pairs ("name" .= name <> "texture" .= texture <> "walkable" .= walkable)
  toEncoding EmptyTile = error "EmptyTile is not serializable"

instance FromJSON Tile where
  parseJSON = withObject "Tile" $ \o -> do
    name <- o .: "name"
    texture <- o .: "texture"
    walkable <- o .: "isWalkable"
    return Tile {tName = name, tTexture = texture, tWalkable = walkable}

data Mode = NoMode | MoveMode [Point] | AttackMode | EnemyMode deriving (Show, Eq)

data World = World
  { wPlayer :: Player,
    wTiles :: [Tile],
    wPictureTileMap :: M.Map String Picture,
    wMaps :: [Map Tile],
    wCurrentMap :: Int,
    wEnemies :: [Enemy],
    wMode :: Mode
  }
  deriving (Show)

(!!!) :: Map a -> Point -> a
Map {mTiles = tiles} !!! (x, y) = (tiles !! floor y) !! floor x

isMapBounded :: Map a -> Point -> Bool
isMapBounded map' (x, y)
  | x < 0 || x >= fromIntegral width = False
  | y < 0 || y >= fromIntegral height = False
  | otherwise = True
  where
    height = length $ mTiles map'
    width = length $ head $ mTiles map'

newState :: (Picture, Picture) -> [Map Tile] -> M.Map String Picture -> [Tile] -> State World
newState (playerPicture, meleeEnemyPicture) maps pictureMap tiles =
  State
    { sData =
        World
          { wPlayer =
              Player (newEntity (4, 0) 10 2 playerPicture) 2,
            wEnemies = [Melee (newEntity (0, 3) 2 1 meleeEnemyPicture) EIdle], -- TODO: load enemies sprites
            wTiles = tiles,
            wPictureTileMap = pictureMap,
            wCurrentMap = 0,
            wMaps = maps,
            wMode = NoMode
          },
      sKeys = S.empty,
      updateTimer = 0.0,
      playerAction = NoAction,
      lastMousePosition = (0, 0)
    }

getTileFromName :: String -> [Tile] -> Tile
getTileFromName name tiles = head $ filter (\tile -> tName tile == name) tiles

createMaps :: [Map Char] -> [Tile] -> [Map Tile]
createMaps charMaps tiles = map createMap charMaps
  where
    createTile :: Char -> Tile
    createTile char = case char of
      'D' -> getTileFromName "Dirt" tiles
      'W' -> getTileFromName "Water" tiles
      ' ' -> EmptyTile
      _ -> undefined

    createMap :: Map Char -> Map Tile
    createMap = fmap createTile

-- map.x = (screen.x / TILE_WIDTH_HALF + screen.y / TILE_HEIGHT_HALF) /2;
-- map.y = (screen.y / TILE_HEIGHT_HALF -(screen.x / TILE_WIDTH_HALF)) /2;
