{-# OPTIONS_GHC -Wall #-}

module Dialogue where

import Identifiers (ActorID, sister, brother)

type Line = String --I miiiiiight change this to Text

data Scipt = Script [Direction] [Script]

data Direction
  = SpeakTo     ActorID [ActorID] Line
  | Speak       ActorID           Line
  | TelepathyTo ActorID [ActorID] Line
  | Telepathy   ActorID           Line
  | Enter       ActorID FilePath
  | Exit        ActorID
  | Wait        Time

cut :: a -> [a]
cut = (:[])

fin :: [a]
fin = []

opening :: Dialogue
opening = Dialogue
  [ 
  ] $ cut $ Dialogue 
