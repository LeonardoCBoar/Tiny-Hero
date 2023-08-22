{-# LANGUAGE ImportQualifiedPost #-}

module Game (State (..), World (..), Player (..), isKeyPressed, insertKey, deleteKey, newState) where

import Data.Set qualified as S
import Graphics.Gloss.Interface.IO.Interact (Key, Point)

data State a = State
  { sData :: a,
    sKeys :: S.Set Key
  }

isKeyPressed :: Key -> State a -> Bool
isKeyPressed k = S.member k . sKeys

insertKey :: Key -> State a -> State a
insertKey k s = s {sKeys = S.insert k (sKeys s)}

deleteKey :: Key -> State a -> State a
deleteKey k s = s {sKeys = S.delete k (sKeys s)}

data Player = Player
  { pPos :: Point
  }

data World = World
  { wPlayer :: Player
  }

newState :: State World
newState =
  State
    { sData =
        World
          { wPlayer =
              Player
                { pPos = (0, 0)
                }
          },
      sKeys = S.empty
    }
