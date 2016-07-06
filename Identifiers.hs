{-# OPTIONS_GHC -Wall #-}

module Identifiers where

import Data.Int (Int64)

newtype RoomID = RoomID String
  deriving (Ord, Eq, Show, Read)

data ActorID = ActorID String
  deriving (Ord, Eq, Read, Show)

newtype Executor = Executor ActorID
  deriving (Ord, Eq, Read, Show)

newtype Target = Target ActorID
  deriving (Ord, Eq, Read, Show)

globalAID :: String -> ActorID
globalAID = ActorID . ("GLOBAL::"++)

anonymous, unsaved_buffer, captain_desoto, professor_dan :: ActorID
stove_dot, penguin, katy_mcgane, kevin_mkII, phoebe, dr_cranston :: ActorID
anonymous       = globalAID "363340126"
unsaved_buffer  = globalAID "623466054"
captain_desoto  = globalAID "184309082"
professor_dan   = globalAID "807786217"
stove_dot       = globalAID "339290775"
penguin         = globalAID "22697100"
katy_mcgane     = globalAID "74073028"
kevin_mkII      = globalAID "564407982"
phoebe          = globalAID "576939762"
dr_cranston     = globalAID "-61835295"
