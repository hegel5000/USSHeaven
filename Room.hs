{-# OPTIONS_GHC -Wall #-}

module Room where

import qualified Data.Set as S

import Identifiers (RoomID)
import Time (RawCD)

type PrintDesc = String

data Room = Room
  { roomName :: String
  , rDesc :: PrintDesc
  , rDoors :: S.Set Door
  , rSize :: RawCD --This is actually the time it takes to get to a door.
}
  
data Door = Door
  { doorName :: String
  , doorTarget :: RoomID
  }
  deriving (Ord, Eq)

