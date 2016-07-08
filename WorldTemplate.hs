{-# OPTIONS_GHC -Wall #-}

module WorldTemplate where

import Identifiers (ActorID, Location)
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
--  Adds the specified namespaces to 'RoomName's and switches them to 'Location's,
--    except when populating the 'rName' fields in 'Room's, 
--    where they just become 'String's.
--  Uses 'DoorTemplate's to generate one or two 'Door's to populate 'Room's as needed.
--  Maps generated 'Location's to generated 'Room's to populate the 'roomsMap' field in the output 'World'.
--  TODO: how do I generate UnitIDs?
bake :: String -> WorldTemplate -> Either String World
bake namespace (roomActTemps, doorTemps) = do
  rooms <- foldM (\ rooms0 doorTemp -> 
      let toLocSafe name rooms = let loc = toLoc name in if M.member loc rooms
            then Right loc
            else err $ "cannot find Location "++show loc++" in DoorTemplate "++show doorTemp
          addDoor sourceName description targetName rooms 
            = liftM2 (\ (sourceLoc, targetLoc) -> M.adjust sourceLoc (\ srcRoom 
                -> srcRoom { rDoors = (Door description targetLoc) : rDoors srcRoom }
              ) rooms ) (toLoc sourceName) (toLoc targetName)
      in 
      case doorTemp of
        (BiDoor lName desc rName)                 -> addDoor lName desc  rName =<< addDoor rName desc  lName rooms0
        (AssymDoor (lName, lDesc) (rName, rDesc)) -> addDoor lName lDesc rName =<< addDoor rName rDesc lName rooms0
        (MonoDoor (lName, lDesc) rName)           -> addDoor lName lDesc rName
    ) (foldl (\ ((name, size, desc), _) -> Room name size desc M.Empty )) doorTemps
  (aIDs, sheets, locs) <- foldM (\ acc ((roomName, _, _), actTemps) 
      -> foldM (\ (aIDs, sheets, locs) ({- TODO: decide on actor templates -})
          -> liftM4 (,,)
            (foldM aIDs actTemps)
            (foldM () sheets)
            (foldM () locs)
        ) acc actTemps
    ) (M.empty, M.empty, M.empty) roomActTemps
  Right $ World 
    { locRoom       = rooms
    , locActorIDs   = 
    , locDeltas     = M.empty
    , locDeltasNew  = M.empty
    , actSheet      = sheets
    , actLocation   = 
    , actDirectors  = M.Empty
    , gloDate       = SinceEpoch $ NanoSeconds 0 --Party like it's 1970, New Years Day!
    , gloRng        = mkStdGen -1 --TODO: decide how USSHeaven handles saving RNG seeds.
    }
  where
    toLoc (RN roomName) room = let loc = Location $ rnamespace++"/"roomName in
      if M.member loc 
    toAID = ActorID . ((rnamespace++"::")++) . sName
    err = Left . ("WorldTemplate.bake: "++)

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
