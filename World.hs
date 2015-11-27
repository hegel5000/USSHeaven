{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

module World where

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe ()
import System.Random (mkStdGen, StdGen)

import Delta (Delta)
import Identifiers (ActorID(..), RoomID, Location (Location, locRoom, locCover))
import Room (Room)
import Sheet (Sheet)
import Time (Date)

--Create a new doorsMap of DoorID to Door.  Have Rooms contain DoorIDs.
data World = World
  { actorsMap     :: M.Map ActorID Sheet
  , roomsMap      :: M.Map RoomID Room
  , aIDToLoc      :: M.Map ActorID Location
  , rIDToAIDs     :: M.Map RoomID (S.Set ActorID)
  , date          :: Date
  , worldDie      :: StdGen
  , deltas        :: M.Map RoomID [Delta]
  , deltasNew     :: M.Map RoomID [Delta]
  , observations  :: M.Map ActorID [Delta]
  , pcDataMap     :: M.Map ActorID PCData
  }

data PCData = NPC | PC { activeControl :: Bool }

getRoomUNSAFE :: RoomID -> World -> Room
getRoomUNSAFE rID = maybe (error "room does not exist!") id . M.lookup rID . roomsMap

aIDToRoomIDUNSAFE :: ActorID -> World -> RoomID
aIDToRoomIDUNSAFE aID@(ActorID idNum)
  = maybe (error $ show idNum++" does not exist! no RoomID for it can be found!") id
  . fmap locRoom . M.lookup aID . aIDToLoc

meleeTargets :: ActorID -> World -> S.Set ActorID
meleeTargets aID world = S.union (sameRoom aID world) (sameDoor aID world)

sameRoom :: ActorID -> World -> S.Set ActorID
sameRoom aID world = maybe S.empty id
  $ fmap locRoom (M.lookup aID $ aIDToLoc world) 
  >>= flip M.lookup (rIDToAIDs world)

sameDoor :: ActorID -> World -> S.Set ActorID
sameDoor aID world = maybe S.empty id
  $ M.lookup aID (aIDToLoc world) -- ActorLocation
  >>= \ actorLocation -> locCover actorLocation -- RoomID
  >>= flip M.lookup (rIDToAIDs world) -- S.Set ActorID
  >>= return . (S.filter (\ u -> atDoorTo (locRoom actorLocation) u world ))

roomOf :: ActorID -> World -> Maybe RoomID
roomOf aID world = locRoom <$> M.lookup aID (aIDToLoc world)

--This is kinda unsafe :P
doorOf :: ActorID -> World -> Maybe RoomID
doorOf aID world = locCover =<< M.lookup aID (aIDToLoc world)

atDoorTo :: RoomID -> ActorID -> World -> Bool
atDoorTo rID aID = maybe False (==rID) . doorOf aID

aIDToRoom :: ActorID -> World -> Maybe Room
aIDToRoom aID world = (\ (Location roomID _) -> M.lookup roomID $ roomsMap world )
  =<< (M.lookup aID $ aIDToLoc world)

aIDToRoomUNSAFE :: ActorID -> World -> Room
aIDToRoomUNSAFE aID = maybe (error $ "ActorID "++show aID++" not present in World (in aIDToRoomUNSAFE)") id
  . aIDToRoom aID

findUniqueActorID :: M.Map ActorID Sheet -> ActorID
findUniqueActorID actors = fuuid 1
  where fuuid n = maybe (ActorID n) (const $ fuuid $ n+1) (M.lookup (ActorID n) actors)

staticWorld :: Date -> [(RoomID, Room)] -> World
staticWorld startTime locations = World { 
    actorsMap = M.empty 
  , roomsMap = M.fromList locations
  , aIDToLoc = M.empty 
  , rIDToAIDs = M.empty 
  , date = startTime
  , worldDie = mkStdGen 19
  , deltas = M.empty
  , deltasNew = M.empty
  , observations = M.empty
  , pcDataMap = M.empty
  }

aDeref :: ActorID -> World -> Sheet
aDeref aID world = maybe noActorIDError id $ M.lookup aID $ actorsMap world
  where noActorIDError = error "attempt to dereference ActorSheet with non-existant ActorID.  I'm not even sure how you could do that!"

aDeref' :: (World -> ActorID) -> World -> Sheet
aDeref' f world = (f >>= aDeref) world

rDeref :: RoomID -> World -> Room
rDeref rID world = maybe (noRIDError rID "rDeref") id $ M.lookup rID $ roomsMap world

noRIDError :: RoomID -> String -> a
noRIDError roomID functionName = error
  $ "non-existant RoomID "++show roomID++" used in "++functionName

noAIDerror :: ActorID -> String -> a
noAIDerror aID functionName = error 
  $ "NON-EXISTANT UNITID "++show aID++" USED IN "++functionName
