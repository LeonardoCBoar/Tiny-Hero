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
    newState,
    isMapBounded,
    (!!!),
    createMaps,
    getTileFromName,
    isTileWalkable,
    findTilesInDistance,
    findWalkableTilesInDistance,
    getMapEnemies,
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
    updateTimer :: Float,
    playerAction :: Action,
    lastMousePosition :: Point,
    showAttackAnimation :: Bool
  }

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
  deriving (Show, Generic)

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
  | isMelee && playerDist <= 1 = enemy {eState = EAttack}
  | isRanged && playerDist <= 8 = enemy {eState = EAttack}
  | otherwise = Melee (moveEntityTo (eEnt enemy) closestTile) EFollow
  where
    world = sData state
    playerDir = pointDiff enemyPos playerPos
    playerDist = manhattanDist playerDir
    enemyPos = ePos $ eEnt enemy
    playerPos = ePos $ pEnt player
    player = wPlayer world
    currentMap = (!! wCurrentMap world) $ wMaps world
    possibleTilesToWalk = findWalkableTilesInDistance world currentMap enemyPos 1
    distances = map (\tPos -> (manhattanDist $ pointDiff tPos playerPos, tPos)) possibleTilesToWalk
    closestTile
      | null distances = enemyPos 
      | otherwise = snd $ minimum distances
    isMelee = isEnemyMelee enemy
    isRanged = isEnemyRanged enemy

    isEnemyMelee (Melee _ _) = True
    isEnemyMelee _ = False
    isEnemyRanged (Ranged _ _) = True
    isEnemyRanged _ = False

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

data MapEnemyDefinition = MapEnemyDefinition
  { enemyType :: String,
    enemyLife :: Int,
    enemyAttack :: Int,
    enemyPos :: Point
  }
  deriving (Show, Generic)

instance ToJSON MapEnemyDefinition where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON MapEnemyDefinition where
  parseJSON = withObject "MapEnemyDefinition" $ \o -> do
    type_ <- o .: "type"
    life <- o .: "life"
    attack <- o .: "attack"
    pos <- o .: "pos"
    return MapEnemyDefinition {enemyType = type_, enemyLife = life, enemyAttack = attack, enemyPos = pos}

createEnemyFromDefinition :: State World -> MapEnemyDefinition -> Enemy
createEnemyFromDefinition state def = case enemyType def of
  "melee" -> Melee (newEntity (enemyPos def) (enemyLife def) (enemyAttack def) (head textures)) EIdle
  "ranged" -> Ranged (newEntity (enemyPos def) (enemyLife def) (enemyAttack def) (textures !! 1)) EIdle
  _ -> undefined
  where
    textures = wEnemyPictures $ sData state

data Map a = Map
  { mTiles :: [[a]],
    enemies :: [MapEnemyDefinition]
  }
  deriving (Show, Generic)

instance (ToJSON a) => ToJSON (Map a) where
  toEncoding = genericToEncoding defaultOptions

instance (FromJSON a) => FromJSON (Map a) where
  parseJSON = withObject "Map" $ \o -> do
    jsonTiles <- o .: "tiles"
    enemies <- o .: "enemies"

    return Map {mTiles = jsonTiles, enemies = enemies}

instance Functor Map where
  fmap f Map {mTiles = tiles, enemies = enemies} = Map {mTiles = map (map f) tiles, enemies = enemies}

instance Applicative Map where
  pure a = Map [[a]] []
  Map [[f]] _ <*> Map values eCount = Map (map (map f) values) eCount
  Map fs _ <*> Map values eCount = Map (zipWith (zipWith ($)) fs values) eCount

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
    wSword :: Picture,
    wEnemyPictures :: [Picture]
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

isEntityInTile :: World -> Point -> Bool
isEntityInTile world tile = tile `elem` map (ePos . eEnt) (wEnemies world) || tile == ePos (pEnt $ wPlayer world)

findTilesInDistance :: Map Tile -> Point -> Float -> [Point]
findTilesInDistance map' (px, py) distance =
  filter
    (isMapBounded map')
    [ (x, y)
      | x <- [px - distance .. px + distance],
        y <- [py - distance .. py + distance],
        abs (x - px) + abs (y - py) <= distance
    ]

findWalkableTilesInDistance :: World -> Map Tile -> Point -> Float -> [Point]
findWalkableTilesInDistance world map' (px, py) distance =
  filter
    (\tilePos -> isTileWalkable (map' !!! tilePos) && not (isEntityInTile world tilePos) )
    (findTilesInDistance map' (px, py) distance)

getMapEnemies :: State World -> Map a -> [Enemy]
getMapEnemies state map' = map (createEnemyFromDefinition state) (enemies map')

newState :: (Picture, Picture) -> [Picture] -> [Map Tile] -> M.Map String Picture -> [Tile] -> State World
newState (playerPicture, sword) enemyPictures maps pictureMap tiles = state {sData = world {wEnemies = enemies}}
  where
    state =
      State
        { sData =
            World
              { wPlayer =
                  Player (newEntity (4, 0) 10 2 playerPicture) 2 1,
                wEnemies = [], -- TODO: load enemies sprites
                wTiles = tiles,
                wPictureTileMap = pictureMap,
                wCurrentMap = 0,
                wMaps = maps,
                wMode = NoMode,
                wSword = sword,
                wEnemyPictures = enemyPictures
              },
          updateTimer = 0.0,
          playerAction = NoAction,
          lastMousePosition = (0, 0),
          showAttackAnimation = False
        }
    world = sData state
    enemies = getMapEnemies state (head maps)

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
