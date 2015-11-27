{-# OPTIONS_GHC -Wall #-}

module Identifiers where

import Data.Int (Int64)

newtype RoomID = RoomID String
  deriving (Ord, Eq, Show, Read)

data Location = Location { locRoom :: RoomID, locCover :: Maybe RoomID }
  deriving (Ord, Eq, Read, Show)

noCover :: RoomID -> Location
noCover rID = Location rID Nothing

data ActorID = ActorID Int64
  deriving (Ord, Eq, Read, Show)

newtype Executor = Executor ActorID
  deriving (Ord, Eq, Read, Show)

newtype Target = Target ActorID
  deriving (Ord, Eq, Read, Show)

brother, sister, captain, doctor, professor :: ActorID
suitor, virus, rival, friend, guardian :: ActorID
brother   = ActorID $ 623466054
sister    = ActorID $ 339290775
captain   = ActorID $ 184309082
doctor    = ActorID $ -61835295
professor = ActorID $ 807786217
suitor    = ActorID $ 363340126
virus     = ActorID $  22697100
rival     = ActorID $  74073028
friend    = ActorID $ 564407982
guardian  = ActorID $ 576939762
