{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Event where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad (void)
import Control.Monad.Reader (runReader)
import Control.Monad.State.Lazy (put, get, lift, modify, runStateT, StateT)
import System.Random (randomR, getStdGen)

import Delta (Delta)
import Identifiers (noCover, ActorID, RoomID)
import Sheet (Sheet)
import View (worldV, View)
import World (
    World(worldDie, aIDToLoc, rIDToAIDs, actorsMap, deltas, deltasNew), findUniqueActorID
  )

type Event = StateT World IO

performEvent :: Event a -> World -> IO (a, World)
performEvent = runStateT

say :: String -> Event ()
say = eventWrapIO . putStrLn

askPlayer :: Event String
askPlayer = eventWrapIO $ getLine

view :: View a -> Event a
view v = return . runReader v =<< get

eventWrapIO :: IO a -> Event a
eventWrapIO = lift

editWorld :: (World -> World) -> Event ()
editWorld = modify

resetStdGen :: Event ()
resetStdGen = do
  dieNew <- eventWrapIO getStdGen
  editWorld (\ world -> world { worldDie = dieNew } )

randomRW :: (Integer, Integer) -> Event Integer
randomRW dieRange = do
  world <- get
  let (roll, nextGen) = randomR dieRange $ worldDie world
  put $ world { worldDie = nextGen }
  return roll 

dn :: Integer -> Event Integer
dn dieMax = randomRW (1, dieMax)

newtype Roll100 = Roll100 Integer

d100 :: Event Roll100
d100 = Roll100 <$> dn 100

insertDelta :: Delta -> [RoomID] -> Event ()
insertDelta delta targetLocs = do
  editWorld $ \ world -> world {
    deltasNew = foldr (\ targetLoc acc 
        -> M.alter (maybe (Just $ [delta]) (Just . (delta:))) targetLoc acc
      ) (deltasNew world) targetLocs
  }

swapDeltas :: Event ()
swapDeltas = editWorld $ \ world -> world {
    deltas = deltasNew world
  , deltasNew = M.empty
  }

doNothing :: Event ()
doNothing = return ()

insertActor :: Sheet -> RoomID -> Event ActorID
insertActor actor targetRID = do
  newUID <- findUniqueActorID . actorsMap <$> view worldV
  editWorld (\ world -> world
      { actorsMap = M.insert newUID actor $ actorsMap world
      , rIDToAIDs = M.insertWith S.union targetRID (S.singleton newUID) $ rIDToAIDs world
      , aIDToLoc = M.insert newUID (noCover targetRID) $ aIDToLoc world
      }
    )
  return newUID

pause :: Event ()
pause = eventWrapIO $ void getLine
