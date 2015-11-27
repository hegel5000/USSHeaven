{-# OPTIONS_GHC -Wall #-}

module Delta where

import ActionRequest (AtkSent)
import Identifiers (Executor(..), Target, RoomID, ActorID)

--This might be an interesting but silly constraint: 
--multi-room actions have different deltas per room.
--For instance, for ranged attacks, the source room could have 
--an attack delta and the target room could have a damage delta.
data Delta = 
    EnterFrom Executor RoomID 
  | ExitTo Executor RoomID
  | AtkDelta Integer Executor Target AtkSent
  deriving (Ord, Eq, Show)

getExitBy :: ActorID -> Delta -> Maybe RoomID
getExitBy targetUID (ExitTo (Executor uID) targetLoc) 
  | targetUID == uID  = Just targetLoc
  | otherwise         = Nothing
getExitBy _ _         = Nothing
