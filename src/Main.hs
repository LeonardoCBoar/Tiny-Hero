module Main (main) where

import Config (fps)
import Game (Player (pPos), State (State, sData), World (World, wPlayer), deleteKey, insertKey, newState)
import Graphics.Gloss (Display (InWindow), Picture, black, circle, color, play, translate, yellow)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), KeyState (Down, Up))

render :: State World -> Picture
render state = color yellow $ translate x y $ circle 6
  where
    (x, y) = pPos $ wPlayer $ sData state

handleEvents :: Event -> State World -> State World
handleEvents (EventKey key keyState _ _) state
  | keyState == Down = insertKey key state
  | keyState == Up = deleteKey key state
  | otherwise = state
handleEvents _ state = state

update :: Float -> State World -> State World
update dt state = state

main :: IO ()
main = do
  let window = InWindow "My Window" (640, 480) (100, 100)

  play window black fps newState render handleEvents update
