{-# OPTIONS_GHC -Wall #-}

import Control.Monad.Reader (ask, Reader)

type View = Reader World

type Action = ActorCxt Event
