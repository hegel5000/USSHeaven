{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module Combinators where

import qualified Data.Set as S

import Control.Monad (join, liftM2)
import Data.Maybe (listToMaybe)

mBind2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
mBind2 tomc ma mb = join $ liftM2 tomc ma mb

boolToEither :: f -> t -> Bool -> Either f t
boolToEither _ t True   = Right t
boolToEither f _ False  = Left f

boolToMaybe :: t -> Bool -> Maybe t
boolToMaybe t True  = Just t
boolToMaybe _ False = Nothing

idBranch :: (a -> b) -> a -> (a, b)
idBranch tob a = (a, tob a)

branchId :: (a -> b) -> a -> (b, a)
branchId tob a = (tob a, a)

maybeToEither :: Maybe a -> Either () a
maybeToEither = maybe (Left ()) Right

greatestWith :: (Ord o, Ord a) => (a -> o) -> S.Set a -> Maybe a
greatestWith toOrd
  = fmap snd . setToMaybe S.findMax . S.map (\ a -> (toOrd a, a))

--I just combined all three of those functions to slightly improve
--efficiency, and I guess also semantic efficiency.
filterSettoList :: (a -> Maybe b) -> S.Set a -> [b]
filterSettoList tomb = S.foldr (\ a acc
    -> maybe acc (:acc) $ tomb a
  ) []

--This is useful if used on a function which crashes on an empty Set.
setToMaybe :: (S.Set a -> b) -> S.Set a -> Maybe b
setToMaybe = makeSafe (not . S.null)

--The second argument is a function which might crash if it has the
--third argument fed into it.
--The first argument is the condition under which the second argument
--will not crash.
--The third argument just gets fed into the second argument.
makeSafe :: (a -> Bool) -> (a -> b) -> a -> Maybe b
makeSafe noCrash tobOrCrash a
  | noCrash a = Just $ tobOrCrash a
  | otherwise = Nothing

findOne :: (a -> Bool) -> S.Set a -> Maybe a
findOne toBool = listToMaybe . S.toList . S.filter toBool

floor0, floor1 :: (Integral n) => n -> n
floor0 n = max n 0 
floor1 n = max n 1

floorAt, capAt :: (Integral n) => n -> n -> n
floorAt = max
capAt = min

onFst :: (a -> c) -> (a, b) -> (c, b)
onFst toC (a, b) = (toC a, b)

onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd toC (a, b) = (a, toC b)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

(*#) :: Integer -> Integer -> Integer
(*#) base expo = base ^ expo

(*^) :: Double -> Integer -> Double
(*^) base expo = base ^ expo

(***) :: Double -> Double -> Double
(***) base expo = base ** expo

normAtan :: Floating a => a -> a
normAtan t = 2 / pi * atan t

viewDub :: Double -> Double -> Double -> String
viewDub mini maxi val
  | val < mini = "v"
  | val >= maxi = "^"
  | otherwise = take 1 . show 
    $ (floor $ 10 * (val - mini) / (maxi - mini) :: Integer)

condAppL :: (a -> Bool) -> (a -> a) -> a -> a
condAppL predicate f a
  | predicate a = f a
  | otherwise = a

condAppR :: (a -> Bool) -> (a -> a) -> a -> a
condAppR predicate f a
  | predicate $ f a = f a
  | otherwise = a
    
condMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
condMap predicate f = fmap (condAppL predicate f)

condAppLOnce :: (a -> Bool) -> (a -> a) -> [a] -> [a]
condAppLOnce predicate f (a:as)
  | predicate a = f a : as
  | otherwise = a : condAppLOnce predicate f as
condAppLOnce _ _ [] = []

firstChar :: String -> Char
firstChar (c:_) = c
firstChar [] = '\n'

{- For instance, oxfordIntersperse "," "and" ["bacon, "egg", "cheese]
 - = "bacon, egg, and cheese"
 -}
oxfordIntersperse :: a -> a -> [a] -> [a]
oxfordIntersperse inter final list = case list of 
  []          -> []
  (a:[])      -> a : []
  (a1:a2:[])  -> a1 : inter : final : a2 : []
  (a1:a2:as)  -> a1 : inter : oxfordIntersperse inter final (a2:as)

{- length (zip as bs)     = 2 * min (length as) (length bs), whereas
 - length (longzip as bs) = length as + length bs
 -}
longZip :: [[a]] -> [[a]] -> [[a]]
longZip (as:ass) (bs:bss) = (as++bs) : longZip ass bss
longZip [] bss = bss
longZip ass [] = ass

condDo :: (Monad m) => Bool -> m () -> m ()
condDo cond m = if cond then m else return ()

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM pred then_ else_ = pred >>= \case
  True -> then_
  False -> else_

tpl23of3 :: (a, b, c) -> (b, c)
tpl23of3 (_, b, c) = (b, c)

mkLeft :: l -> Maybe r -> Either l r
mkLeft l Nothing  = Left l
mkLeft _ (Just r) = Right r

rmLeft :: Either l r -> Maybe r
rmLeft (Left _)   = Nothing
rmLeft (Rigth r)  = Just r
