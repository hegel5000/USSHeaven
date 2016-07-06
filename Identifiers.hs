{-# OPTIONS_GHC -Wall #-}

module Identifiers where

import Data.Int (Int64)

newtype RoomID = RoomID String
  deriving (Ord, Eq, Show, Read)

data ActorID = ActorID Int64
  deriving (Ord, Eq, Read, Show)

newtype Executor = Executor ActorID
  deriving (Ord, Eq, Read, Show)

newtype Target = Target ActorID
  deriving (Ord, Eq, Read, Show)

brother, sister, captain, doctor, professor :: ActorID
suitor, virus, rival, friend, guardian :: ActorID
unsaved_buffer  = ActorID $ 623466054
stove_dot       = ActorID $ 339290775
captain_desoto  = ActorID $ 184309082
doctor_cranston = ActorID $ -61835295
professor_dan   = ActorID $ 807786217
anonymous       = ActorID $ 363340126
penguin         = ActorID $  22697100
katy_mcgane     = ActorID $  74073028
kevin_mkII      = ActorID $ 564407982
phoebe          = ActorID $ 576939762
