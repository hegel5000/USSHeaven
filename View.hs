{-# OPTIONS_GHC -Wall #-}

module View where

import qualified Data.Map as M
import qualified Data.Set as S

import qualified SetMap as SM

import Control.Monad (foldM, liftM2)
import Control.Monad.Reader (ask, Reader)
import Data.Functor ()
import Data.Maybe ()

import Combinators (makeSafe)
import Delta (Delta)
import Identifiers (RoomID, Location(Location, locRoom), ActorID)
import Room (Room(rDoors, roomName), Door(doorTarget))
import Sheet (getCD, Sheet(actorName))
import Time (CDType, Date)
import World (aIDToRoomUNSAFE, noAIDerror, getRoomUNSAFE
  , meleeTargets , aDeref, PCData (PC), World (pcDataMap, aIDToLoc, date, deltas)
  )
--
--I might try Lenses at some point, though.

type View = Reader World

worldV :: View World
worldV = ask

sheetV :: ActorID -> View Sheet
sheetV uID = aDeref uID <$> worldV

roomV :: RoomID -> View Room
roomV rID = getRoomUNSAFE rID <$> worldV

doorsV :: ActorID -> View (S.Set Door)
doorsV aID = fmap (rDoors . aIDToRoomUNSAFE aID) worldV

locationV :: ActorID -> View Location
locationV aID = maybe (noAIDerror aID "getLocation") id . M.lookup aID . aIDToLoc <$> worldV

actorsInMeleeV :: ActorID -> View (S.Set ActorID)
actorsInMeleeV uID = fmap (meleeTargets uID) worldV

bestActorV :: (Ord o) => (Sheet -> o) -> ActorID -> View (Maybe ActorID)
bestActorV toOrd searcher = liftM2 (\ world
      -> fmap snd . makeSafe (not . null) maximum
      . fmap (\ uID -> (\ actor -> (toOrd actor, uID)) $ aDeref uID world )
      . S.toList
    ) (worldV) (actorsInMeleeV searcher)

bestActor1V :: (Ord o) => (Sheet -> o) -> ActorID -> ActorID -> View ActorID
bestActor1V toOrd searcher previousBest = liftM2 (\ world
      -> snd . maximum
      . fmap (\ uID -> (\ actor -> (toOrd actor, uID)) $ aDeref uID world)
      . (previousBest:) . S.toList
    ) (worldV) (actorsInMeleeV searcher)

deltasV :: RoomID -> View [Delta]
deltasV location = fmap (maybe [] (id)
  . M.lookup location . deltas) worldV

deltasPartyV :: [ActorID] -> View (S.Set Delta)
deltasPartyV = foldM (\ acc uID 
    -> fmap (\ ds -> S.union (S.fromList ds) acc ) $ deltasV =<< (locRoom <$> locationV uID)
  ) S.empty

timeV :: View Date
timeV = worldV >>= return . date

cooldownV :: CDType -> ActorID -> View Double
cooldownV cdType uID = fmap (getCD cdType) $ sheetV uID

isReadyV :: CDType -> ActorID -> View Bool
isReadyV cdType = fmap (<0) . cooldownV cdType

pcsV :: View [ActorID]
pcsV = M.foldrWithKey (\ key a acc -> if isPC a then key : acc else acc ) [] . pcDataMap <$> worldV
  where
    isPC (PC _) = True
    isPC _      = False

roomsPCsV :: View (SM.SetMap RoomID ActorID)
roomsPCsV = fmap SM.fromList
  $ mapM (\ pc -> (\ loc -> (locRoom loc, pc)) <$> locationV pc)
  =<< pcsV

addrV :: ActorID -> View String
addrV account = liftM2 
  (\ username location -> username++"@"++location )
  (show . actorName <$> sheetV account) (dirV =<< locationV account)

dirV :: Location -> View String
dirV (Location rID maybeCoverRID) = liftM2 (++)
  (roomStrV rID)
  (maybe (return "") (roomStrV) maybeCoverRID)

roomStrV :: RoomID -> View String
roomStrV = fmap (("/"++) . roomName) . roomV

adjacentRoomsV :: RoomID -> View (S.Set RoomID)
adjacentRoomsV = fmap (S.map doorTarget . rDoors) . roomV
