{-# OPTIONS_GHC -Wall #-}

module Sheet where

import qualified Data.Map as M
import qualified Data.Set as S

import Combinators (fi)
import Stat ( Stat, printActorName, ActorName, ActorArchitecture, DmgType, Defense, WpnType
  , ConditionsBlock, StatName(..), statVal, unWounded, toDefense
  )
import Time(CDType(..))

data Sheet = Sheet
  { actorName :: ActorName
  , uArch :: ActorArchitecture

  , aggression, beauty
  , design,     willpower
  , technique,  friendliness
  , grace,      security
  , efficiency, intelligence
  , energy,   psi
  , shield, proxy
  , humanity
  :: Stat

  , absorptions, defs :: M.Map DmgType Defense
  , skls :: M.Map WpnType Stat

  , conditions :: ConditionsBlock
  
  , cdPhys, cdPSI :: Double
} deriving (Ord, Eq, Show, Read)

editStat :: StatName -> (Stat -> Stat) -> Sheet -> Sheet
editStat sn toStat sheet = setStat sn (toStat $ getStat sn sheet) sheet

getStatVal :: StatName -> Sheet -> Integer
getStatVal stN = statVal . getStat stN

getStatVals :: S.Set StatName -> Sheet -> M.Map StatName Integer
getStatVals stats sheet = M.fromList
  $ fmap (\ statName -> (statName, getStatVal statName sheet))
  $ S.toList stats

getStat :: StatName -> Sheet -> Stat
getStat sn = case sn of
  AGG -> aggression;  DES -> design;    TEC -> technique;    GRA -> grace;    EFF -> efficiency
  BEA -> beauty;      WIL -> willpower; FRI -> friendliness; SEC -> security; INT -> intelligence
  HUM -> humanity; PSI -> psi; NRG -> energy

getCD :: CDType -> Sheet -> Double
getCD CDPhys = cdPhys
getCD CDPSI  = cdPSI

getCDReduceDivisor :: CDType -> Sheet -> Double
getCDReduceDivisor cdType sheet = case cdType of
  CDPhys  -> (getStat' EFF)
  CDPSI   -> (getStat' INT)
  where
    getStat' statName = fi $ getStatVal statName sheet

--Not very useful, but it's easier to write editStat based on this.
setStat :: StatName -> Stat -> Sheet -> Sheet
setStat sn stat sheet = case sn of
  AGG -> sheet { aggression = stat }
  DES -> sheet { design = stat }
  TEC -> sheet { technique = stat }
  GRA -> sheet { grace = stat }
  EFF -> sheet { efficiency = stat }
  BEA -> sheet { beauty = stat }
  WIL -> sheet { willpower = stat }
  FRI -> sheet { friendliness = stat }
  SEC -> sheet { security = stat }
  INT -> sheet { intelligence = stat }
  HUM -> sheet { humanity = stat }
  PSI -> sheet { psi = stat }
  NRG -> sheet { energy = stat }

--This data constructor takes in Integers instead of Stats.  It's much
--easier to type out a character sheet this way.  If you want your Actor
--to be wounded when it enters the game, put it through some damage
--functions a few times.
data ActorTemplate = UT
  { uArch' :: ActorArchitecture

  , aggression', beauty'
  , design',     willpower'
  , technique',   friendliness'
  , grace',      security'
  , efficiency', intelligence'
  , humanity',   psi'
  , energy'
  :: Integer

  , defs', absorbtions' :: [(DmgType, (String, String, Integer))]
  , skls' :: [(WpnType, Integer)]
} deriving (Ord, Eq, Show, Read)

fromTemplate :: ActorName -> ActorTemplate -> Sheet
fromTemplate name ut = U 
  name
  (uArch' ut) 
  (unWounded $ aggression' ut)  (unWounded $ beauty' ut)
  (unWounded $ design' ut)      (unWounded $ willpower' ut)
  (unWounded $ technique' ut)   (unWounded $ friendliness' ut)
  (unWounded $ grace' ut)       (unWounded $ security' ut)
  (unWounded $ efficiency' ut)  (unWounded $ intelligence' ut)
  (unWounded $ humanity' ut)    (unWounded $ psi' ut)
  (unWounded $ energy' ut)
  (M.map toDefense $ M.fromList $ defs' ut)
  (M.map toDefense $ M.fromList $ absorbtions' ut)
  (M.map unWounded $ M.fromList $ skls' ut)
  M.empty
  --
  1.00 1.00

