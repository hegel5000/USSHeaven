{-# OPTIONS_GHC -Wall #-}

module DmgCalc where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Reader (ask)
import Data.Functor ((<$>))

import Action (Roll100(..))
import AtkBlks
import Identifiers (Executor(..))
import MathUtil (capAt, fi, floor1, idBranch)
import Stat (insertWound, Wound(..))
import UnitSheet (editStat, UnitSheet(..), getStatVal, getStatVals)
import UnitView (sheetUV, UnitView)

sendAtk :: AtkPlus -> String -> AtkPrintMode -> UnitView AtkSent
sendAtk blk woundDescription printMode = do
  sheet <- sheetUV
  user <- Executor <$> ask
  return $ AtkSent {
      ac' = ac blk
    , hitStatsUsed = getStatVals (hitStats blk) sheet
    , pwrStatsUsed = getStatVals (pwrStats blk) sheet
    , wndDesc = woundDescription
    , prtMode = printMode
    , atkUser = user
    }

statSum :: S.Set SN -> UnitSheet -> Integer
statSum stats sheet = sum 
  $ fmap (\ statName -> getStatVal statName sheet)
  $ S.toList stats

hit :: AtkSent -> Integer
hit blk 
  = (baseHit $ ac' blk) + (sum $ fmap snd $ M.toList $ hitStatsUsed blk)
--
dodUsed :: AtkSent -> UnitSheet -> Integer
dodUsed blk = statSum (dodStats $ ac' blk)
--
hitLevel :: AtkSent -> Roll100 -> UnitSheet -> HitLv
hitLevel blk (Roll100 roll) trgtSheet = HitLv
  $ (+1) . div 100 . subtract roll 
  $ hit blk - dodUsed blk trgtSheet

power :: AtkSent -> Integer
power blk 
  = (basePwr $ ac' blk) 
  + (sum $ fmap snd $ M.toList $ pwrStatsUsed blk)

defUsed :: AtkSent -> UnitSheet -> Integer
defUsed blk = statSum (defStats $ ac' blk)

dmgRcvd :: AtkSent -> HitLv -> UnitSheet -> Integer
dmgRcvd blk (HitLv hitLv) sheet
  | hitLv <= 0 = 0 --Miss
  | otherwise = floor1
      $ (hasFeat SmashCrit ((capAt 2 hitLv)*) $ power blk) 
      - (hasFeat SharpshootCrit (div hitLv) $ defUsed blk sheet)
  where
    hasFeat feat f = if S.member feat $ features $ ac' blk then f else id

splitDmg :: [SN] -> Integer -> [(SN, Integer)]
splitDmg trgts dmg = (\ (plus1s, base)
    -> fmap (idBranch $ const $ partialDmg + 1) plus1s
    ++ fmap (idBranch $ const partialDmg) base
  ) $ splitAt (fi $ dmg `mod` targetCount) trgts
  where
    targetCount = fi $ length trgts
    partialDmg = dmg `div` targetCount

splitDmgs :: [SN] -> [[SN]] -> Integer -> UnitSheet -> [(SN, Integer)]
splitDmgs [] [] _ _ =  error 
  $ "Attack block with empty target stat lists encountered!" 
splitDmgs backStats [] dmg _ = splitDmg backStats dmg
splitDmgs backStats (trgts:targetss) dmg sheet
  | rollover == 0 = dmgs
  | otherwise = dmgs ++ splitDmgs trgts targetss rollover sheet
  where
    (rollover, dmgs)
      | dmg < lowestStatVal = (0, splitDmg allStats dmg)
      | otherwise = (dmg - lowestStatVal, splitDmg allStats lowestStatVal)
    lowestStatVal = foldr1 min 
      $ fmap (\ statName -> getStatVal statName sheet) backStats
    allStats = backStats++trgts

--So when actually dealing with damage, the type a should be set to DmgType 
--and x as as Integer or however it is you're doing damage.
dmgConsolidate :: (Ord a, Num x) => [(a, x)] -> M.Map a x
dmgConsolidate = foldl (\ acc (a, x) 
    -> M.alter (Just . maybe 0 (+x)) a acc
  ) M.empty

--Where damage is a verb here.
damageSheet :: Integer -> (Integer -> Wound) -> [[SN]] -> UnitSheet -> UnitSheet
damageSheet damage toWound trgtStatss trgtSheet
  = foldl (\ accSheet (statName, dmgToStat)
    -> editStat statName (insertWound $ toWound dmgToStat) accSheet
  ) trgtSheet
  $ M.toList $ dmgConsolidate $ splitDmgs [] trgtStatss damage trgtSheet
