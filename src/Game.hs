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
    Damageable (..),
    EnemyState (..),
    Scene (..),
    isValidAction,
    newState,
    isMapBounded,
    (!!!),
    createMaps,
    getTileFromName,
    isTileWalkable,
    findTilesInDistance,
    findWalkableTilesInDistance,
    getMapEnemies,
    isEntityInTile,
    sumEnemiesAttack,
    pointDiff,
    manhattanDist,
    restartGame,
  )
where

import Data.Aeson hiding (Key)
import Data.Map qualified as M
import Data.Set qualified as S
import GHC.Generics
import Graphics.Gloss.Interface.IO.Interact (Key (..), Picture, Point)

data Scene = Menu | Game | GameOver | Win deriving (Show, Eq)

data State a = State
  { sData :: a,
    sKeys :: S.Set Key,
    updateTimer :: Float,
    playerAction :: Action,
    lastMousePosition :: Point,
    showAttackAnimation :: Bool,
    currentScene :: Scene
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
    eStats :: Stats,
    eTexture :: Picture
  }
  deriving (Show)

pointDiff :: Point -> Point -> Point
pointDiff (originX, originY) (targetX, targetY) = (targetX - originX, targetY - originY)

manhattanDist :: Point -> Int
manhattanDist (x, y) = abs (round x) + abs (round y)

newEntity :: Point -> Int -> Int -> Picture -> Entity
newEntity startPos startLife attackValue = Entity startPos (Stats startLife startLife attackValue)

data Player = Player {pEnt :: Entity, pMaxMoveDistance :: Int, pMaxAttackDistance :: Int}
  deriving (Show)

data EnemyState = EIdle | EFollow | EAttack | ERangedAttack deriving (Show, Eq)

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

sumEnemiesAttack :: [Enemy] -> Int
sumEnemiesAttack mapEnemies =
  sum
    [ attack $ eStats $ eEnt enemy
      | enemy <- mapEnemies,
        eState enemy == EAttack || eState enemy == ERangedAttack
    ]

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
    enemyLifeDef <- o .: "life"
    enemyAttackDef <- o .: "attack"
    pos <- o .: "pos"
    return MapEnemyDefinition {enemyType = type_, enemyLife = enemyLifeDef, enemyAttack = enemyAttackDef, enemyPos = pos}

createEnemyFromDefinition :: State World -> MapEnemyDefinition -> Enemy
createEnemyFromDefinition state def = case enemyType def of
  "melee" -> Melee (newEntity (enemyPos def) (enemyLife def) (enemyAttack def) (head textures)) EIdle
  "ranged" -> Ranged (newEntity (enemyPos def) (enemyLife def) (enemyAttack def) (textures !! 1)) EIdle
  _ -> undefined
  where
    textures = wEnemyPictures $ sData state

data Map a = Map
  { mTiles :: [[a]],
    mEnemies :: [MapEnemyDefinition],
    mPlayerPos :: Point
  }
  deriving (Show, Generic)

instance (ToJSON a) => ToJSON (Map a) where
  toEncoding = genericToEncoding defaultOptions

instance (FromJSON a) => FromJSON (Map a) where
  parseJSON = withObject "Map" $ \o -> do
    jsonTiles <- o .: "tiles"
    mapEnemies <- o .: "enemies"
    playerPos <- o .: "playerPos"

    return Map {mTiles = jsonTiles, mEnemies = mapEnemies, mPlayerPos = playerPos}

instance Functor Map where
  fmap f Map {mTiles = tiles, mEnemies = mapEnemies, mPlayerPos = playerPos} =
    Map {mTiles = map (map f) tiles, mEnemies = mapEnemies, mPlayerPos = playerPos}

instance Applicative Map where
  pure a = Map [[a]] [] (0, 0)
  Map [[f]] _ _ <*> Map values eCount pPos = Map (map (map f) values) eCount pPos
  Map fs _ _ <*> Map values eCount pPos = Map (zipWith (zipWith ($)) fs values) eCount pPos

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
    wFireball :: Picture,
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

isEnemyInTile :: World -> Point -> Bool
isEnemyInTile world tile = tile `elem` map (ePos . eEnt) (wEnemies world)

isEntityInTile :: World -> Point -> Bool
isEntityInTile world tile = isEnemyInTile world tile || tile == ePos (pEnt $ wPlayer world)

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

getMapEnemies :: State World -> Map a -> [Enemy]
getMapEnemies state map' = map (createEnemyFromDefinition state) (mEnemies map')

newState :: (Picture, Picture, Picture) -> [Picture] -> [Map Tile] -> M.Map String Picture -> [Tile] -> State World
newState (playerPicture, sword, fireball) enemyPictures maps pictureMap tiles = state {sData = world {wEnemies = mapEnemies}}
  where
    state =
      State
        { sData =
            World
              { wPlayer =
                  Player (newEntity playerPos 10 2 playerPicture) 2 1,
                wEnemies = [],
                wTiles = tiles,
                wPictureTileMap = pictureMap,
                wCurrentMap = 0,
                wMaps = maps,
                wMode = NoMode,
                wSword = sword,
                wFireball = fireball,
                wEnemyPictures = enemyPictures
              },
          updateTimer = 0.0,
          playerAction = NoAction,
          lastMousePosition = (0, 0),
          showAttackAnimation = False,
          sKeys = S.empty,
          currentScene = Menu
        }
    world = sData state
    playerPos = mPlayerPos $ head maps
    mapEnemies = getMapEnemies state (head maps)

getTileFromName :: String -> [Tile] -> Tile
getTileFromName name tiles = head $ filter (\tile -> tName tile == name) tiles

restartGame :: State World -> State World
restartGame state = state {sData = world {wPlayer = player', wCurrentMap = currentMap', wEnemies = enemies'}, currentScene = Game}
  where
    world = sData state
    player' = Player (newEntity playerPos 10 2 (eTexture $ pEnt $ wPlayer world)) 2 1
    currentMap' = 0
    enemies' = getMapEnemies state (head $ wMaps world)
    playerPos = mPlayerPos $ head $ wMaps world

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
