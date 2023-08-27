{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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
    Damageable (..),
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
    findTilesInDistance,
    findWalkableTilesInDistance,
  )
where

import Data.Aeson hiding (Key)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import GHC.Generics
import Graphics.Gloss.Interface.IO.Interact (Key (..), Picture, Point)

data State a = State
  { sData :: a,
    sKeys :: S.Set Key,
    updateTimer :: Float,
    playerAction :: Action,
    lastMousePosition :: Point,
    showAttackAnimation :: Bool
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
  { maxLife :: Int,
    life :: Int,
    attack :: Int
  }
  deriving (Show)

data Entity = Entity
  { ePos :: Point,
    eId :: Int, -- TODO: Implement unique IDs
    eStats :: Stats,
    eTexture :: Picture
  }
  deriving (Show)

pointDiff :: Point -> Point -> Point
pointDiff (originX, originY) (targetX, targetY) = (targetX - originX, targetY - originY)

manhattanDist :: Point -> Int
manhattanDist (x, y) = abs (round x) + abs (round y)

newEntity :: Point -> Int -> Int -> Picture -> Entity
newEntity startPos startLife attack = Entity startPos 0 (Stats startLife startLife attack)

moveEntity :: Entity -> Point -> Entity
moveEntity entity (dx, dy) = entity {ePos = (x + dx, y + dy)}
  where
    pos = ePos entity
    x = fst pos
    y = snd pos

moveEntityTo :: Entity -> Point -> Entity
moveEntityTo entity (x, y) = entity {ePos = (x, y)}

moveEntityTowards :: Entity -> Point -> Entity -- TODO: Check collisions
moveEntityTowards entity (distanceX, distanceY)
  | abs distanceX >= abs distanceY = moveEntity entity (distanceX / abs distanceX, 0)
  | otherwise = moveEntity entity (0, distanceY / abs distanceY)

data Player = Player {pEnt :: Entity, pMaxMoveDistance :: Int, pMaxAttackDistance :: Int}
  deriving (Show)

updatePlayer :: Action -> Player -> Int -> Player
updatePlayer (Move dir) (Player ent moveDistance attackDistance) damageValue =
  Player (fromJust $ damage (moveEntity ent dir) damageValue) moveDistance attackDistance
updatePlayer _ (Player ent moveDistance attackDistance) damageValue =
  Player (fromJust $ damage ent damageValue) moveDistance attackDistance

data EnemyState = EIdle | EFollow | EAttack deriving (Show, Eq)

data Enemy
  = Melee {eEnt :: Entity, eState :: EnemyState}
  | Ranged {eEnt :: Entity, eState :: EnemyState}
  deriving (Show)

class Damageable a where
  damage :: a -> Int -> Maybe a

instance Damageable Entity where
  damage entity value
    | newLife <= 0 = Nothing
    | otherwise = Just $ entity {eStats = stats'}
    where
      stats = eStats entity
      newLife = life stats - value
      stats' = stats {life = life stats - value}

updateEnemy :: State World -> Enemy -> Enemy
updateEnemy state enemy
  | playerDist == 0 = error "Impossible collision"
  | playerDist <= 1 = enemy {eState = EAttack}
  -- \| otherwise = Melee (moveEntityTowards (eEnt enemy) playerDir) EFollow -- TODO: Support ranged
  | otherwise = Melee (moveEntityTo (eEnt enemy) closestTile) EFollow
  where
    world = sData state
    playerDir = pointDiff enemyPos playerPos
    playerDist = manhattanDist playerDir
    enemyPos = ePos $ eEnt enemy
    playerPos = ePos $ pEnt player
    player = wPlayer world
    currentMap = (!! wCurrentMap world) $ wMaps world
    possibleTilesToWalk = findWalkableTilesInDistance currentMap enemyPos 1
    distances = map (\tPos -> (manhattanDist $ pointDiff tPos playerPos, tPos)) possibleTilesToWalk
    closestTile = snd $ minimum distances

sumEnemiesAttack :: [Enemy] -> Int
sumEnemiesAttack enemies = sum [attack $ eStats $ eEnt enemy | enemy <- enemies, eState enemy == EAttack]

updateEnemyStats :: Point -> [Enemy] -> [Enemy]
updateEnemyStats _ [] = []
updateEnemyStats (x, y) (e : es)
  | ePos (eEnt e) == (x, y) = case newEntity of
      Nothing -> updateEnemyStats (x, y) es
      Just entity -> e {eEnt = entity} : es
  | otherwise = e : updateEnemyStats (x, y) es
  where
    newEntity = damage (eEnt e) 1

updateWorld :: State World -> World
updateWorld state = world {wPlayer = player, wEnemies = enemies, wMode = mode}
  where
    world = sData state
    pAction = playerAction state
    mode =
      if pAction == NoAction
        then NoMode
        else EnemyMode
    enemies = case pAction of
      Attack atkPos -> updateEnemyStats atkPos $ map (updateEnemy state) (wEnemies world)
      _ -> map (updateEnemy state) (wEnemies world)
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

data Mode = NoMode | MoveMode [Point] | AttackMode [Point] | EnemyMode deriving (Show, Eq)

data World = World
  { wPlayer :: Player,
    wTiles :: [Tile],
    wPictureTileMap :: M.Map String Picture,
    wMaps :: [Map Tile],
    wCurrentMap :: Int,
    wEnemies :: [Enemy],
    wMode :: Mode,
    wSword :: Picture
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

findTilesInDistance :: Map Tile -> Point -> Float -> [Point]
findTilesInDistance map' (px, py) distance =
  filter
    (isMapBounded map')
    [ (x, y)
      | x <- [px - distance .. px + distance],
        y <- [py - distance .. py + distance],
        abs (x - px) + abs (y - py) <= distance
    ]

findWalkableTilesInDistance :: Map Tile -> Point -> Float -> [Point]
findWalkableTilesInDistance map' (px, py) distance =
  filter
    (\tilePos -> isTileWalkable (map' !!! tilePos))
    (findTilesInDistance map' (px, py) distance)

newState :: (Picture, Picture, Picture) -> [Map Tile] -> M.Map String Picture -> [Tile] -> State World
newState (playerPicture, meleeEnemyPicture, sword) maps pictureMap tiles =
  State
    { sData =
        World
          { wPlayer =
              Player (newEntity (4, 0) 10 2 playerPicture) 2 1,
            wEnemies = [Melee (newEntity (0, 3) 2 1 meleeEnemyPicture) EIdle], -- TODO: load enemies sprites
            wTiles = tiles,
            wPictureTileMap = pictureMap,
            wCurrentMap = 0,
            wMaps = maps,
            wMode = NoMode,
            wSword = sword
          },
      sKeys = S.empty,
      updateTimer = 0.0,
      playerAction = NoAction,
      lastMousePosition = (0, 0),
      showAttackAnimation = False
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
