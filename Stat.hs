{-# OPTIONS_GHC -Wall #-}

module Stat where

import Data.Maybe(fromMaybe)

import qualified Data.Map as M
import qualified Data.Set as S

addWound :: Integer -> S.Set DmgType -> String -> Stat -> Stat
addWound amnt types descr (Stat maxVal wounds)
  = Stat maxVal $ (Wnd amnt types descr):wounds

insertWound :: Wound -> Stat -> Stat
insertWound wound (Stat maxVal wounds) = Stat maxVal $ wound:wounds

data Stat = Stat Integer [Wound]
  deriving (Ord, Eq, Show, Read)

statVal :: Stat -> Integer
statVal (Stat maxi wounds) = maxi - sum (map dmgAmount wounds)

unWounded :: Integer -> Stat
unWounded maxVal = Stat maxVal []

data Wound = Wnd {
  dmgAmount :: Integer, 
  dmgTypes :: S.Set DmgType, 
  dmgDescr :: String
} deriving (Ord, Eq, Show, Read)

data Defense = Defense
  { penetrateDesc :: String
  , resistDesc :: String
  , defenseAmnt :: Stat
} deriving (Ord, Eq, Show, Read)

toDefense :: (String, String, Integer) -> Defense
toDefense (s1, s2, n) = Defense s1 s2 $ unWounded n

type LethalDivs = M.Map ActorArchitecture Integer

findFactor, findTerm :: (Ord a, Num x) => a -> M.Map a x -> x
findFactor key m = maybe 1 id $ M.lookup key m
findTerm key m = fromMaybe 0 $ M.lookup key m

data ActorName = 
    Anonymous
  | CatherineMcgane
  | CharlesThePenguin
  | DanielFitzpatrick
  | Doomguy
  | ElizabethCranston
  | KevinMkII
  | MariaDeSoto
  | StoveDeSoto
  | UnsavedBufferDeSoto
  deriving (Ord, Eq, Show, Read)

printActorName :: ActorName -> String
printActorName un = case un of
  Anonymous           -> "anonymous"
  CatherineMcgane     -> "Cathy"
  CharlesThePenguin   -> "Charles the Penguin"
  DanielFitzpatrick   -> "Professor Dan"
  Doomguy             -> "Doomguy"
  ElizabethCranston   -> "Dr. Cranston"
  KevinMkII           -> "Kevin Mk.II"
  MariaDeSoto         -> "Captain DeSoto"
  StoveDeSoto         -> "Stove."
  UnsavedBufferDeSoto -> "UNSAVED_BUFFER"

data ActorArchitecture = 
    Amorphous --Anonymous
  | Cat --UNSAVED_BUFFER
  | Captain --Captain DeSoto
  | Classic --Professor Dan
  | Cranston --Dr. Cranston
  | Doom
  | Mcgane --Catherine Mcgane
  | Penguin --Charles the
  | RISK --KevinMkII
  | Stove --Stove.
  deriving (Ord, Eq, Show, Read)

data DmgType =
    Disruption | Sanity | Bio
  | Heat | Electric | Cold | Tidal
  | Magnetic | Uncertainty
  deriving (Ord, Eq, Show, Read)

data WpnType = 
    Swinging | Stabbing | Unarmed
  | Hitscan | Explosive | Projectile
  | Psychic
  deriving (Ord, Eq, Show, Read)

data StatName = 
    AGG | DES | TEC | GRA | EFF
  | BEA | WIL | FRI | SEC | INT
  | HUM | PSI | NRG
  deriving (Ord, Eq, Show, Read)

type ConditionsBlock = M.Map Condition Integer

data Condition = 
    Sleepy
  | Numb
  | Confused
  | Crying
  | Braindead
  deriving (Ord, Eq, Show, Read)
