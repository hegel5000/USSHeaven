{-# OPTIONS_GHC -Wall #-}

module SetMap where

import qualified Data.Map as M
import qualified Data.Set as S

type SetMap k a = M.Map k (S.Set a)

insert :: (Ord k, Ord a) => k -> a -> SetMap k a -> SetMap k a
insert k a = M.alter (maybe (Just $ S.singleton a) (Just . S.insert a)) k

fromList :: (Ord k, Ord a) => [(k, a)] -> SetMap k a
fromList = foldr (uncurry insert) M.empty

fromListMulti :: (Ord k, Ord a) => [(k, [a])] -> SetMap k a
fromListMulti = foldr (\ (k, as) -> \ setMap -> foldr (insert k) setMap as) M.empty
