{-# OPTIONS_GHC -Wall #-}

module Action where

import qualified Data.Map as M

import Control.Monad.Reader (lift, filterM, runReaderT)

import ActorContext (ActorCxt, toActorCxt, withActorID, toActorCxt)
import Attribute (addrAtt, cooldownAtt, visibleTo)
import Event (Event, view, eventWrapIO, editWorld, say)
import Time (CDType(CDPhys, CDPSI))
import Sheet (Sheet(cdPhys, cdPSI))
import View (View, pcsV)
import World (World(actorsMap), noAIDerror, actorsMap)

type Action = ActorCxt Event

viewU :: ActorCxt View a -> ActorCxt Event a
viewU unitView = toActorCxt $ view . withActorID unitView

actWrapIO :: IO a -> Action a
actWrapIO = lift . eventWrapIO

editSheet :: (Sheet -> Sheet) -> Action ()
editSheet f = toActorCxt $ \ uID -> editWorld (\ world -> world {
      actorsMap = M.alter (maybe 
          (noAIDerror uID "editActor")
          (Just . f)
        ) uID $ actorsMap world
    }
  )

addCD :: CDType -> Double -> Action ()
addCD cdType cd = editCD cdType (+cd)

editCD :: CDType -> (Double -> Double) -> Action ()
editCD cdType f = do
  cd0 <- viewU (cooldownAtt cdType) 
  editSheet $ \ unit -> case cdType of
    CDPhys -> unit { cdPhys = f cd0 }
    CDPSI  -> unit { cdPSI  = f cd0 }

promptPrint :: String -> Action ()
promptPrint s = lift . say =<< (\ directory -> directory++" >> "++s) <$> viewU addrAtt

--(command prompt style) print to player character viewers
prtPCsVwr :: String -> Action ()
prtPCsVwr s = sequence_ . map (lift . runReaderT (promptPrint s))
  =<< filterM (viewU . visibleTo) =<< (viewU . lift) pcsV
