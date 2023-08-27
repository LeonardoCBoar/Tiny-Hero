module GameObjects
    (
        Player(..),
        Enemy(..),
        Action(..),
        isValidAction,
        newEntity,
        updatePlayer
    )
where

import Graphics.Gloss.Interface.IO.Interact (Point)
import Game(Action(..))

data Action = NoAction | Move Point | Attack Point deriving (Show)

isValidAction :: Action -> Bool
isValidAction NoAction = False
isValidAction _ = True

data Stats = Stats
  {
    maxLife :: Integer,
    life :: Integer,
    attack :: Integer
  } deriving (Show)

data Entity = Entity
  {
    ePos :: Point,
    eId :: Integer, --TODO: Implement unique IDs
    eStats :: Stats
  } deriving (Show)

newEntity :: Point -> Integer -> Integer -> Entity
newEntity startPos startLife attack = Entity startPos 0 (Stats startLife startLife attack)

moveEntity :: Entity -> Point -> Entity
moveEntity entity (dx, dy) = entity {ePos = (x + dx, y + dy)}
  where
    pos = ePos entity
    x = fst pos
    y = snd pos

data Player = Player {pEnt :: Entity}
  deriving (Show)

updatePlayer :: Action -> Player -> Player
updatePlayer (Move dir) (Player ent) = Player $ moveEntity ent dir
updatePlayer _ player = player

data Enemy = Melee {eEnt :: Entity} | Ranged { eEnt :: Entity}