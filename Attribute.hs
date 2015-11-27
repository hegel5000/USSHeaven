{-# OPTIONS_GHC -Wall #-}

module Attribute where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Reader (filterM, lift, liftM2, ask, runReaderT)
import Data.Maybe (listToMaybe, catMaybes)

import ActorContext(ActorCxt)
import Combinators (makeSafe)
import Delta (getExitBy, Delta)
import Identifiers (ActorID, Location(Location), RoomID, Location(Location, locRoom))
import Room (Door, Room(rDoors))
import Stat (ActorName)
import Time (CDType)
import Sheet (getCD, getCDReduceDivisor, actorName, Sheet)
import View (addrV, pcsV, roomStrV, roomV, View, worldV)
import World (deltas, meleeTargets, noAIDerror, uDeref, aIDToRoomUNSAFE, aIDToLoc)

type Attribute = ActorCxt View

--Unify this naming scheme with the one used in View.actorV
sheetAtt :: Attribute Sheet
sheetAtt = ask >>= \ uID -> uDeref uID <$> lift worldV

actorIDAtt :: Attribute ActorID
actorIDAtt = ask

nameAtt :: Attribute ActorName
nameAtt = fmap actorName sheetAtt

doorsAtt :: Attribute (S.Set Door)
doorsAtt = ask >>= \ aID -> rDoors . aIDToRoomUNSAFE aID <$> lift worldV

locationAtt :: Attribute Location
locationAtt = ask >>= \ aID 
  -> maybe (noAIDerror aID "locationAtt") (id) . M.lookup aID . aIDToLoc <$> lift worldV

roomIDAtt :: Attribute RoomID
roomIDAtt = locRoom <$> locationAtt

roomAtt :: Attribute Room
roomAtt = lift . roomV =<< roomIDAtt

actorsInMeleeAtt :: Attribute (S.Set ActorID)
actorsInMeleeAtt = ask >>= \ uID -> meleeTargets uID <$> lift worldV

bestActorAtt :: (Ord o) => (Sheet -> o) -> Attribute (Maybe ActorID)
bestActorAtt toOrd = liftM2 (\ world
      -> fmap snd . makeSafe (not . null) maximum
      . fmap (\ uID -> (\ actor -> (toOrd actor, uID)) $ uDeref uID world)
      . S.toList
    ) (lift worldV) (actorsInMeleeAtt)

bestActor1V :: (Ord o) => (Sheet -> o) -> ActorID -> Attribute ActorID
bestActor1V toOrd previousBest = liftM2 (\ world
      -> snd . maximum
      . fmap (\ uID -> (\ actor -> (toOrd actor, uID)) $ uDeref uID world)
      . (previousBest:) . S.toList
    ) (lift worldV) (actorsInMeleeAtt)

deltasAtt :: Attribute [Delta]
deltasAtt = roomIDAtt >>= \ location -> (maybe [] (id) . M.lookup location . deltas) <$> lift worldV

followDestAtt :: ActorID -> Attribute (Maybe RoomID)
followDestAtt target = (listToMaybe . catMaybes . fmap (getExitBy target)) <$> deltasAtt

cooldownAtt :: CDType -> Attribute Double
cooldownAtt cdType = getCD cdType <$> sheetAtt

isReadyAtt :: CDType -> Attribute Bool
isReadyAtt cdType = (<0) <$> cooldownAtt cdType

cdReduceDivisorAtt :: CDType -> Attribute Double
cdReduceDivisorAtt cdType = getCDReduceDivisor cdType <$> sheetAtt

addrAtt :: Attribute String
addrAtt = lift . addrV =<< ask

--This gives the address string of the input UID relative to the context UID.
relAddrAtt :: ActorID -> Attribute String
relAddrAtt other = relDirAtt =<< (lift $ runReaderT locationAtt other)

relDirAtt :: Location -> Attribute String
relDirAtt posOther@(Location roomOther mCoverOther) = do
  pos@(Location room _) <- locationAtt
  roomStr <- if room == roomOther then return "~/" else lift $ roomStrV roomOther
  coverStr <- if pos == posOther then return "" else lift $ maybe (return "") (roomStrV) mCoverOther
  return $ roomStr++coverStr

--Four cases for visibility of action: 
--An action is performed by a Actor in one of the following 
--rooms relative to the viewer: 
--same room as viewer
--taking cover at door to actor's room
--room that viewer is taking cover at a door to
--taking cover at door to room that viewer is taking cover at a door to
visibleTo :: ActorID -> Attribute Bool
visibleTo viewer = do
  (Location room        cover) <- locationAtt
  (Location roomViewer  coverViewer) <- lift $ runReaderT locationAtt viewer
  return $ room == roomViewer 
    || maybe False (==roomViewer) cover
    || maybe False (==room) coverViewer
    || maybe False id (liftM2 (==) cover coverViewer)

viewingPCsAtt :: Attribute [ActorID]
viewingPCsAtt = filterM visibleTo =<< lift pcsV
