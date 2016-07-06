{-# OPTIONS_GHC -Wall #-}

module WorldTemplate where

type TempRoomID

type WorldTemplate  = [(Room, [ActorID])] 
type RoomTemplate   = (TempRoomID, Size, Description, [DoorTemplate RoomID]) 
type DoorTemplate   = (DoorName, TempRoomID, Description)

lit :: String -> View String
lit = return

data VirtualPlazaTempRIDs = 
    Garden | Patio | Parlor 
  | RedCorridor | Guestroom1 | Guestroom2 | Guestroom3 | Conservatory
  | BlueCorridor | Theater | Plaza
  | Diningroom | Kitchen
  | GreenCorridor | PianoRoom | Office
  deriving Show

data VirtualPlazaTempUIDs = 
    TopiaryMenelaus | TopiaryAchilles | KevinMkIIForm1
  deriving Show

virtualPlaza :: WorldTemplate VirtualPlazaTempRIDs VirtualPlazaTempUIDs
virtualPlaza = 
  [ ( ( Garden
      , 10.0
      , lit "The grass and shrubs are green.  The sun is warm and pleasant."
      , [ ( "terrace steps"
          , Patio
          , lit "They lead up."
          )
        ]
      )
    , [TopiaryMenelaus, TopiaryAchilles]
    )
  , ( ( Patio
      , 3.5
      , lit "The tiles have a pleasant, sunny motif.  The sunless sky is a blue-grey."
      , [ ( "terrace steps"
          , Garden
          , lit "They lead down."
          )
        , ( "glass door"
          , Parlor
          , lit "Surrounded by gently wafting curtains."
          ) 
        ]
      )
    , []
    )
  , ( ( Parlor
      , lit "Carpet, wallpaper, and ceiling are a sylvan green."
      , 3.0
      , [ ( "glass door"
          , Patio
          , lit "Sunlight shines through."
          )
        , ( "oak door"
          , PianoRoom
          , maybe "It's thick." (++" can be heard on the other side.") <$> pianoProbe
          )
          ( "pine door"
          , Office
          , lit "
          )
        ]
      )
    , []
    )
  ]
