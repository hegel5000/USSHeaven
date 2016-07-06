{-# OPTIONS_GHC -Wall #-}

module WorldTemplate where

import Identifiers (ActorID)
import Room (Room)
import Sheet (Sheet)
import Time (RawCD(..))
import World (World)

newtype RoomName = RN String
  deriving (Show)
type WorldTemplate = ([(RoomTemplate, [ActorTemplate])], [DoorTemplate])
type RoomTemplate = (RoomName, RawCD, String)
data DoorTemplate = 
    BiDoor RoomName String RoomName
  | AssymDoor (RoomName, String) (RoomName, String)
  | MonoDoor (RoomName, String) RoomName
  deriving (Show)
type ActorTemplate = Sheet

-- |'Bake' does a few things.
--  Adds the specified namespaces to 'RoomName's and switches them to 'RoomID's,
--    except when populating the 'rName' fields in 'Room's, 
--    where they just become 'String's.
--  Uses 'DoorTemplate's to generate one or two 'Door's to populate 'Room's as needed.
--  Maps generated 'RoomID's to generated 'Room's to populate the 'roomsMap' field in the output 'World'.
--  TODO: how do I generate UnitIDs?
bake :: String -> WorldTemplate -> Either String World
bake namespace (rooms, doors) = do
  (actorsMap1, roomsMap1, aIDsToRIDs <- foldM (\ ac
      -> 
    ) M.empty rooms

  Right $ World 
    { wRooms        = roomsMap'
    , wSheets     :: M.Map ActorID Sheet
    , wLocations      :: M.Map ActorID RoomID
    , wContents     :: M.Map RoomID (S.Set ActorID)
    , date          = SinceEpoch $ NanoSeconds 0 --Party like it's 1970, January 1st!
    , worldDie      = mkStdGen 10 --TODO: decide how USSHeaven handles saving RNG seeds.
    , deltas        = M.empty
    , deltasNew     = M.empty
    , observations  = M.empty
    , pcDataMap     = M.Empty
    }
  where
    mangle (RN roomName) = namespace++"/"roomName

virtualPlaza :: WorldTemplate
virtualPlaza = 
  ( [ ( (RN "Garden", RawCD 10.0, "The grass and shrubs are green.  The sun is warm and pleasant.")
      , [TopiaryMenelaus, TopiaryAchilles]
      )
    , ( (RN "Patio", RawCD 3.5, "The tiles have a pleasant, sunny motif.  The sunless sky is a blue-grey.")
      , []
      )
    , ( (RN "Parlor", RawCD 3.0, "Carpet, wallpaper, and ceiling are a sylvan green.")
      , []
      )
    , ( (RN "Office", RawCD 1.0, "Hardwood everything.") 
      , []
      )
    , ( (RN "Blue Hallway", RawCD 0.9, "Cheap fluorescent lights, stained carpet, scratched paint.")
      , []
      )
    ]
  , [ AssymDoor (RN "Garden", "terrace steps up") (RN "Patio", "terrace steps down")
    , BiDoor (RN "Patio") "wafting-curtained glass door" (RN "Parlor")
      , BiDoor (RN "Parlor") "thick oak door" (RN "Piano Room")
      , BiDoor (RN "Parlor") "red pine door" (RN "Office")
      , BiDoor (RN "Parlor") "narrow arch" (RN "Blue Hallway")
        , BiDoor (RN "Blue Hallway") "college dorm door" (RN "Room 114")
          , BiDoor (RN "Room 114") "bathroom door" (RN "Kevin's Photography Lab")
            , MonoDoor (RN "Photography Lab", "photo of flowers") (RN "Garden")
    ]
  )
