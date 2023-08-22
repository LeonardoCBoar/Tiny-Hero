module Main (main) where

import Config (fps)
import Game (State (State), World (World), newState)
import Graphics.Gloss (Display (InWindow), Picture, black, circle, color, play, yellow)
import Graphics.Gloss.Interface.IO.Game (Event)

render :: State World -> Picture
render state = color yellow $ circle 80

handleEvents :: Event -> State World -> State World
handleEvents event state = state

update :: Float -> State World -> State World
update dt state = state

main :: IO ()
main = do
  let window = InWindow "My Window" (640, 480) (100, 100)

  play window black fps newState render handleEvents update
