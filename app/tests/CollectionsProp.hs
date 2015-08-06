{-# LANGUAGE ScopedTypeVariables #-}
module CollectionsProp (tprops) where

import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
--import Test.QuickCheck.Property as Prop
import Data.List

--import qualified Data.Introhs.Util as Util
{-
-- Data.Functor.<$> ; GHC.Base.<*>   -- prefix fmap --> infix <$>
genTup2 :: Gen a -> Gen a -> Gen (a, a)
genTup2 gen0 gen1 = (,) <$> gen0 <*> gen1

genTup3 :: Gen a -> Gen a -> Gen a -> Gen (a, a, a)
genTup3 gen0 gen1 gen2 = (,,) <$> gen0 <*> gen1 <*> gen2
-}
--epsilon = 0.001
--(lst, revlst) = ([0..4], reverse (enumFromTo 0 4))

isOrdered, isRevOrdered :: (Eq a, Show a, Ord a) => [a] -> Bool
isOrdered lst = case lst of
    x:y:rst -> x <= y && isOrdered (y:rst)
    _ -> True

isRevOrdered lst = case lst of
    x:y:rst -> x >= y && isRevOrdered (y:rst)
    _ -> True


propCons :: (Eq a, Show a) => a -> [a] -> Bool
propCons x xs = x == head (x:xs)

propNull :: (Eq a, Show a) => [a] -> Bool
propNull xs = (0 == length xs) == null xs

propEqual :: (Eq a, Show a) => [a] -> Property
propEqual xs = (not . null) xs ==> fst $ foldl (\(a, (m:ms)) e -> 
    (a && m == e, ms)) (True, ys) xs
  where ys = map id xs

propNotequal :: (Eq a, Show a) => [a] -> [a] -> Property
propNotequal xs ys = (not . null) xs ==> (not . null) ys ==> (xs /= ys) ==>
    (length xs <= length ys) ==> (fst $ foldl (\(a, ms) e -> case ms of
        n:ns -> (a && n == e, ns)
        _ -> (False, [])) (True, xs) ys) == False

propAppend :: (Eq a, Show a) => [a] -> [a] -> Bool
propAppend xs ys = (drop (length xs) (xs ++ ys)) == ys &&
    (take (length xs) (xs ++ ys)) == xs

propRevRev :: (Eq a, Show a) => [a] -> Bool
propRevRev xs = (reverse . reverse) xs == xs

propFilter, propRemove :: (Eq a, Show a) => (a -> Bool) -> [a] -> Bool
propFilter pred1 xs = all pred1 $ filter pred1 xs
propRemove pred1 xs = all pred1 $ fst (partition pred1 xs)

propMap :: (Eq a, Show a) => (a -> a) -> [a] -> Property
propMap f xs = (not . null) xs ==> fst $ foldl (\(a, (m:ms)) e -> 
    (a && m == (f e), ms)) (True, ys) xs
  where ys = map f xs


propSortIsOrdered, propRevSortIsRevOrdered :: [Int] -> Bool
propSortIsOrdered = isOrdered . sort
propRevSortIsRevOrdered = isRevOrdered . reverse . sort

{-
genC = (suchThat (elements [0..20]) even)
genTup2Int = genTup2 (elements [0..10]) (elements [(-18)..(-1)])
genTup3Int = genTup3 genC genC genC

propComb1 = (within 100) . forAll genTup2Int . uncurry
propComb2 = forAll genTup3Int . Util.uncurry3
propComb3 = forAll (listOf1 $ elements [' '..'~'])
-}

tprops = map (uncurry testProperty) $ 
    [("propCons", property (propCons :: Int -> [Int] -> Bool))
    , ("propCull", property (propNull :: [Int] -> Bool))
    , ("propEqual", property (propEqual :: [Int] -> Property))
    , ("propNotequal", property (propNotequal :: [Int] -> [Int] -> Property))
    , ("propAppend", property (propAppend :: [Int] -> [Int] -> Bool))
    , ("propRevRev", property (propRevRev :: [Int] -> Bool))
    , ("propFilter", property (propFilter even :: [Int] -> Bool))
    , ("propRemove", property (propRemove even :: [Int] -> Bool))
    , ("propMap", property (propMap (\e -> e + 1) :: [Int] -> Property))
    , ("propSortIsOrdered", property (propSortIsOrdered :: [Int] -> Bool))
    , ("propRevSortIsRevOrdered", property (propRevSortIsRevOrdered :: [Int] -> Bool))
    ]
