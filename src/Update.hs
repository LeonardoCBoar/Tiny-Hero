module Update (updateWorld, refillEntityLife, updateGame) where

import Config (updateInterval)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (fromMaybe)
import Game
  ( Action (..),
    Damageable (damage),
    Enemy (..),
    EnemyState (EAttack, EFollow, ERangedAttack),
    Entity (..),
    Mode (EnemyMode, NoMode),
    Player (..),
    Scene (GameOver, Win),
    State (..),
    Stats (..),
    World (..),
    findWalkableTilesInDistance,
    getMapEnemies,
    isEntityInTile,
    isValidAction,
    manhattanDist,
    pointDiff,
    sumEnemiesAttack,
  )
import Graphics.Gloss (Point)

moveEntity :: Point -> Entity -> Entity
moveEntity point entity = entity {ePos = finalPos}
  where
    (px, py) = ePos entity
    finalPos = bimap (px +) (py +) point

refillEntityLife :: Entity -> Entity
refillEntityLife entity = entity {eStats = entityStats {life = refilledLife}}
  where
    entityStats = eStats entity
    refilledLife = maxLife entityStats

updatePlayer :: [Enemy] -> Float -> State World -> Player
updatePlayer enemies _ state = case pAction of
  Move dir -> player {pEnt = if isValidMove dir then movedPlayer dir else playerEntity}
  _ -> player {pEnt = playerEntity}
  where
    pAction = playerAction state
    player = wPlayer $ sData state
    enemiesDamage = sumEnemiesAttack enemies
    movedPlayer dir = moveEntity dir playerEntity
    movedPos dir = ePos $ movedPlayer dir
    isValidMove dir = movedPos dir `notElem` map (ePos . eEnt) enemies

    playerEntity =
      fromMaybe
        (pEnt player) {eStats = (eStats $ pEnt player) {life = 0}}
        (damage (pEnt player) enemiesDamage)

updateEnemies :: Float -> State World -> [Enemy]
updateEnemies _ state = map updateEnemy enemies
  where
    enemies = wEnemies $ sData state

    updateEnemy :: Enemy -> Enemy
    updateEnemy enemy = case enemy of
      Melee _ _ ->
        if distanceToPlayer <= 1
          then enemy {eState = EAttack}
          else enemy {eEnt = moveEntity moveDir enemyEntity, eState = EFollow}
      Ranged _ _ ->
        if distanceToPlayer <= 8
          then enemy {eState = ERangedAttack}
          else enemy {eEnt = moveEntity moveDir enemyEntity}
      where
        enemyEntity = eEnt enemy
        enemyPos = ePos enemyEntity
        playerPos = ePos $ pEnt $ wPlayer $ sData state
        distanceToPlayer = manhattanDist $ pointDiff enemyPos playerPos

        currentMap = (!! wCurrentMap (sData state)) $ wMaps (sData state)
        possibleTilesToWalk = filter (not . isEntityInTile (sData state)) (findWalkableTilesInDistance currentMap enemyPos 1)
        distances = map (\tPos -> (manhattanDist $ pointDiff tPos playerPos, tPos)) possibleTilesToWalk
        moveDir = pointDiff enemyPos closestTile
        closestTile
          | null distances = enemyPos
          | otherwise = snd $ minimum distances

updateEnemyAtPosition :: Point -> [Enemy] -> [Enemy]
updateEnemyAtPosition _ [] = []
updateEnemyAtPosition pos (e : es)
  | ePos (eEnt e) == pos = case entity of
      Just entity' -> e {eEnt = entity'} : es
      Nothing -> es
  | otherwise = e : updateEnemyAtPosition pos es
  where
    entity = damage (eEnt e) 1

updateWorld :: Float -> State World -> World
updateWorld dt state = sData state'
  where
    mode =
      if playerAction state == NoAction
        then NoMode
        else EnemyMode

    player' = updatePlayer enemies dt state

    enemies = updateEnemies dt state
    enemies' = case playerAction state of
      Attack attackPos -> updateEnemyAtPosition attackPos enemies
      _ -> enemies

    state'
      | isValidAction (playerAction state) =
          state
            { sData =
                (sData state)
                  { wPlayer = player',
                    wEnemies = enemies',
                    wMode = mode
                  },
              playerAction = NoAction,
              updateTimer = 0
            }
      | otherwise = state

updateGame :: Float -> State World -> State World
updateGame dt state
  | life (eStats playerEnt) <= 0 = state {currentScene = GameOver}
  | wCurrentMap world >= length (wMaps world) = state {currentScene = Win}
  | isValidAction action = state {sData = updateWorld dt state, updateTimer = 0, playerAction = NoAction}
  | showAttackAnimation state && updateTimer state >= updateInterval = state {showAttackAnimation = False, updateTimer = 0}
  | null $ wEnemies world =
      state
        { sData =
            world
              { wEnemies = newEnemies,
                wCurrentMap = wCurrentMap world + 1,
                wPlayer = (wPlayer world) {pEnt = refillEntityLife playerEnt}
              }
        }
  | otherwise = state {updateTimer = curUpdateTimer, sData = world}
  where
    world = sData state
    playerEnt = pEnt $ wPlayer $ sData state
    action = playerAction state
    curUpdateTimer = updateTimer state + dt
    mapIndex = (wCurrentMap world + 1) `mod` length (wMaps world)
    map' = (!! mapIndex) $ wMaps world
    newEnemies = getMapEnemies state map'
