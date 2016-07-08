{-# OPTIONS_GHC -Wall #-}

module Room where

import qualified Data.Set as S

import Identifiers (Address)
import Time (RawCD)

-- |You might notice that these 'Room's don't contain 'Actor.Actor's or 'Identifiers.ActorID's.
--  Take a look at 'World' to see how an 'Actor.Actor' can be \'in\' a 'Room'.
data Room = Room
  { rName :: String -- ^ Appears when in this 'Room' and along with 'dNames' of 'Doors' leading to this 'Room'.
  , rSize :: RawCD -- ^ About the time (in seconds) it takes to get from some part of this 'Room' to the 'Door'.
  , rDesc :: String -- ^ Appears when in this 'Room' and when this 'Room' is examined from an adjacent 'Room'.
  , rDoors :: [Door] -- ^ These are the 'Door's that lead /from/ this 'Room'.
  }
  deriving (Ord, Eq, Show, Read)
  
-- |Doors are wholly contained inside of the rooms they lead /from/.
--  Individual Door object aren't supposed to appear anywhere except in the rDoors field.
--  A 'Door' in one 'Room' /usually/ needs a corresponding 'Door' in another room.
--  In the world of USSHeaven, things such as security cameras or paintings on walls can act as one-way 'Door's.
--  Note that while a 'Room' directly contains 'Door's, 'Door's only reference 'Room's (through 'Address's).
data Door = Door
  { dDesc :: String -- ^ Door description.  Always appears with the 'rName' of the 'Room' corresponding to 'dTarget'.
  , dTarget :: Address -- ^ Corresponds to the particular 'Room' this 'Door' leads to (see 'World').
  }
  deriving (Ord, Eq, Show, Read)

