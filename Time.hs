{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Time where

import Data.Ratio ((%))

seconds :: (RealFrac s) => s -> Time
seconds = NanoSeconds . round . (*nsPerSec)

newtype Time = NanoSeconds Integer
  deriving (Eq, Ord, Read, Show, Num, Enum, Integral)

instance Real Time where
  toRational (NanoSeconds ns) = ns%nsPerSec

newtype Date = SinceEpoch Time
  deriving (Eq, Ord, Read, Show)

--This is how many seconds it would take to do something with
--a speed stat of 100 (where a speed stat is either EFF or INT).
newtype RawCD = RawCD Double 
  deriving (Ord, Eq, Show, Read) 

data CDType = CDPhys | CDPSI
  deriving (Ord, Eq, Show, Read)

nsPerSec :: (Num ns) => ns
nsPerSec = 1000000000
