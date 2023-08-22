{-# LANGUAGE ImportQualifiedPost #-}

module Game (State (..), World (..), hasKey, insertKey, deleteKey, newState) where

import Data.Set qualified as S
import Graphics.Gloss.Interface.IO.Interact (Key)

data State a = State
  { sData :: a,
    sKeys :: S.Set Key
  }

hasKey :: Key -> State a -> Bool
hasKey k = S.member k . sKeys

insertKey :: Key -> State a -> State a
insertKey k s = s {sKeys = S.insert k (sKeys s)}

deleteKey :: Key -> State a -> State a
deleteKey k s = s {sKeys = S.delete k (sKeys s)}

data World = World

newState :: State World
newState = State World S.empty
