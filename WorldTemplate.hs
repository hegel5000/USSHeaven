{-# OPTIONS_GHC -Wall #-}

module WorldTemplate where

import Identifiers (ActorID)
import Room (Room)
import Sheet (Sheet)
import World (World)

type WorldTemplate = [(RoomTemplate, [ActorTemplate])]
type RoomTemplate = (String, Double, String, [DoorTemplate])
type DoorTemplate = (String, String)
type ActorTemplate = Sheet

bake :: String -> WorldTemplate -> World
bake = 

virtualPlaza :: WorldTemplate
virtualPlaza = 
  [ ( ( "Garden", 10.0, "The grass and shrubs are green.  The sun is warm and pleasant."
      , [ ("terrace steps up" , "Patio")
        ]
      )
    , [TopiaryMenelaus, TopiaryAchilles]
    )
  , ( ( "Patio", 3.5, "The tiles have a pleasant, sunny motif.  The sunless sky is a blue-grey."
      , [ ("terrace steps down", "Garden")
        , ("wafting-curtained glass door", "Parlor") 
        ]
      )
    , []
    )
  , ( ( "Parlor", 3.0, "Carpet, wallpaper, and ceiling are a sylvan green."
      , [ ("sun-streaming wafting-curtained glass door", "Patio")
        , ("thick oak door", "Piano Room")
          ("red pine door", "Office")
        ]
      )
    , []
    )
  ]
