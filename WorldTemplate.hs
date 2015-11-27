{-# OPTIONS_GHC -Wall #-}

module WorldTemplate where

type RoomTemplate temp = (temp, (String, Maybe String, String), Double, View String, [DoorTemplate])

stdThe :: String -> (String, Maybe String, String)
stdThe shortRoomName = ("in", Just "the", shortRoomName)

lit :: (Monad m) => String -> m String
lit = return

VirtualPlazaTempRIDs = 
    Garden | Patio | Parlor 
  | RedCorridor | Guestroom1 | Guestroom2 | Guestroom3 | Conservatory
  | BlueCorridor | Theater | Plaza
  | Diningroom | Kitchen
  | GreenCorridor | Pianoroom | Office

VirtualPlazaTempUIDs = 
    Shrublord | KevinMkIIForm1

virtualPlaza :: WorldTemplate VirtualPlazaTempRIDs VirtualPlazaTempUIDs
  [ ( Garden
    , stdThe "garden"
    , 10.0
    , lit "Green, well-arrenged, and soldierlike, the hedgerows stand watch over Kevin's tasteful mansion from down the terraces.  The sun is warm and pleasant."
    , [ ( "terrace steps"
        , Patio
        , lit "Steps lead to a patio behind the mansion.  "
        )
      ]
    )
  , ( Patio
    , ("on", Just "the", "patio")
    , 3.5
    , lit "The tiles have a pleasant, sunny motif, contrasting the pale blue but somehow dim midday sky.  "
    , [ ( "terrace steps"
        , Garden
        , lit "Terrace steps lead to the hedge garden.  The sun shines more brightly down there."
        )
      , ( "glass door"
        , Parlor
        , lit "Pale curtains waft gently on either side of an open glass door which leads to the dark green parlour."
        ) 
      ]
    )
  , ( Parlor
    , stdThe "parlor"
    , lit "The upholstery, carpeting, wallpaper, and the mural on the ceiling are heavily patterned, but seem to meld into a dark, possibly sylvan green."
    , 3.0
    , [ ( "glass door"
        , Patio
        , lit "Sunlight difuses through the glass door's curtains like it would through a white, sideways forest canopy on one side of a living room."
        )
      ]
    )
  ]
