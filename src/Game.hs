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
    Action (..),
    Map (..),
    Tile (..),
    isValidAction,
    updatePlayer,
    updateWorld,
    isKeyPressed,
    insertKey,
    deleteKey,
    newState,
    (!!!),
  )
where

import Data.Aeson hiding (Key)
import Data.Set qualified as S
import Debug.Trace (trace)
import GHC.Generics
import Graphics.Gloss.Interface.IO.Interact (Key (..), Picture, Point, SpecialKey (..))

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

data Stats = Stats
  { maxLife :: Integer,
    life :: Integer,
    attack :: Integer
  }
  deriving (Show)

data Entity = Entity
  { ePos :: Point,
    eId :: Integer, -- TODO: Implement unique IDs
    eStats :: Stats
  }
  deriving (Show)

pointDiff :: Point -> Point -> Point
pointDiff (originX, originY) (targetX, targetY) = (targetX - originX, targetY - originY)

manhattanDist :: Point -> Integer
manhattanDist (x, y) = abs (round x + round y)

newEntity :: Point -> Integer -> Integer -> Entity
newEntity startPos startLife attack = Entity startPos 0 (Stats startLife startLife attack)

moveEntity :: Entity -> Point -> Entity
moveEntity entity (dx, dy) = entity {ePos = (x + dx, y + dy)}
  where
    pos = ePos entity
    x = fst pos
    y = snd pos

moveEntityTowards :: Entity -> Point -> Entity -- TODO: Check collisions
moveEntityTowards entity (distanceX, distanceY)
  | abs distanceX >= abs distanceY = do
      trace
        (show distanceX ++ " > " ++ show distanceY)
        moveEntity
        entity
        (distanceX / abs distanceX, 0)
  | otherwise = do
      trace
        (show distanceX ++ " < " ++ show distanceY)
        moveEntity
        entity
        (0, distanceY / abs distanceY)

data Player = Player {pEnt :: Entity}
  deriving (Show)

updatePlayer :: Action -> Player -> Player
updatePlayer (Move dir) (Player ent) = Player $ moveEntity ent dir
updatePlayer _ player = player

data EnemyState = EIdle | EFollow | EAttack -- TODO

data Enemy = Melee {eEnt :: Entity} | Ranged {eEnt :: Entity} deriving (Show)

updateEnemy :: State World -> Enemy -> Enemy
updateEnemy (State world _ _ _) enemy
  | playerDist == 0 = error "Impossible colision"
  | playerDist == 1 = enemy -- TODO: Cause damage to player
  | otherwise = Melee $ moveEntityTowards (eEnt enemy) playerDir -- TODO: Support ranged
  where
    playerDist = manhattanDist playerDir
    playerDir = pointDiff enemyPos playerPos
    enemyPos = ePos $ eEnt enemy
    playerPos = ePos $ pEnt player
    player = wPlayer world

updateWorld :: State World -> World
updateWorld state = world {wPlayer = player, wEnemies = enemies}
  where
    world = sData state
    player = updatePlayer (playerAction state) (wPlayer world)
    enemies = map (updateEnemy state) (wEnemies world)

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

data Tile = Tile {tTexture :: Picture} deriving (Show)

data World = World
  { wPlayer :: Player,
    wTiles :: [Tile],
    wMaps :: [Map Tile],
    wEnemies :: [Enemy]
  }
  deriving (Show)

(!!!) :: Map a -> Point -> a
Map {mTiles = tiles} !!! (x, y) = (tiles !! floor y) !! floor x

newState :: [Tile] -> State World
newState tiles =
  State
    { sData =
        World
          { wPlayer =
              Player (newEntity (0, 0) 10 2),
            wEnemies = [Melee (newEntity (10, 10) 2 1)],
            wTiles = tiles,
            wMaps =
              [ Map
                  { mTiles =
                      [ [head tiles, head tiles, head tiles],
                        [head tiles, head tiles, head tiles],
                        [head tiles, head tiles, head tiles]
                      ]
                  }
              ]
          },
      sKeys = S.empty,
      updateTimer = 0.0,
      playerAction = NoAction
    }
