module Events (handleMenuEvents, handleGameEvents, handleGameOverEvents, handleWinEvents) where

import Config (updateInterval)
import Game (Action (Attack, Move), Entity (ePos), Mode (AttackMode, MoveMode, NoMode), Player (pEnt, pMaxAttackDistance, pMaxMoveDistance), Scene (..), State (..), World (..), findWalkableTilesInDistance, isEntityInTile, restartGame)
import Graphics.Gloss.Interface.IO.Game
import Renderer (screenPositionToWorldPosition)

handleMenuEvents :: Event -> State World -> State World
handleMenuEvents (EventKey (SpecialKey KeySpace) Down _ _) state =
  state {currentScene = Game}
handleMenuEvents _ state = state

handleGameOverEvents :: Event -> State World -> State World
handleGameOverEvents (EventKey (SpecialKey KeySpace) Down _ _) state = restartGame state
handleGameOverEvents _ state = state

handleWinEvents :: Event -> State World -> State World
handleWinEvents (EventKey (SpecialKey KeySpace) Down _ _) state = restartGame state
handleWinEvents _ state = state

handleGameEvents :: Event -> State World -> State World
handleGameEvents event state
  | updateTimer state < updateInterval = state
  | otherwise = case event of
      EventKey (Char 'm') Down _ _ ->
        let mode = wMode world
            playerPos = ePos $ pEnt $ wPlayer world
            maxDistance = fromIntegral $ pMaxMoveDistance $ wPlayer world
            walkableTilesInMoveRange =
              filter
                (not . isEntityInTile world)
                (findWalkableTilesInDistance currentMap playerPos maxDistance)
         in case mode of
              MoveMode _ -> state {sData = world {wMode = NoMode}}
              _ -> state {sData = world {wMode = MoveMode walkableTilesInMoveRange}}
      EventKey (Char 'a') Down _ _ ->
        let mode = wMode world
            playerPos = ePos $ pEnt $ wPlayer world
            maxDistance = fromIntegral $ pMaxAttackDistance $ wPlayer world
            walkableTilesInAttackRange = findWalkableTilesInDistance currentMap playerPos maxDistance
         in case mode of
              AttackMode _ -> state {sData = world {wMode = NoMode}, showAttackAnimation = True}
              _ -> state {sData = world {wMode = AttackMode walkableTilesInAttackRange}}
      EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY) ->
        let (x, y) = screenPositionToWorldPosition (mouseX, mouseY)
            (px, py) = ePos $ pEnt $ wPlayer world
         in case wMode world of
              MoveMode walkableTiles ->
                if (x, y) `elem` walkableTiles
                  then state {lastMousePosition = (x, y), playerAction = Move (x - px, y - py)}
                  else state {lastMousePosition = (x, y)}
              AttackMode possibleAttackTiles ->
                if (x, y) `elem` possibleAttackTiles
                  then state {lastMousePosition = (x, y), playerAction = Attack (x, y)}
                  else state {lastMousePosition = (x, y)}
              _ -> state
      _ -> state
  where
    world = sData state
    currentMap = (!! wCurrentMap world) $ wMaps world
