{-# LANGUAGE ScopedTypeVariables #-}
module SequenceopsProp (tprops) where

import Data.List
import Data.Word

import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
--import Test.QuickCheck.Property as Prop

import qualified Data.Introhs.Util as Util
import Data.Introhs.Practice.Sequenceops

--epsilon = 0.001

propReverse :: (Eq a, Show a) => [a] -> Bool
propReverse xs = foldl (\a f -> a && f xs == ans) True 
    [reverseR, reverseI]
  where ans = reverse xs

propFindIndex :: (Eq a, Show a) => (a -> Bool) -> [a] -> Bool
propFindIndex pred1 xs = foldl (\a f -> a && f 0 pred1 xs == ans) True 
    [findIndexR, findIndexI]
  where ans = findIndex pred1 xs

-- Data.Functor.<$> ; GHC.Base.<*>   -- prefix fmap --> infix <$>
--genTup2Int = (,) <$> (elements ([1..20] :: [Float])) <*>
--    (elements ([2..10] :: [Float]))


tprops = map (uncurry testProperty) $ 
    [("reverse == List.reverse", property (propReverse :: [Int] -> Bool))
    , ("findIndex == List.findIndex", property (propFindIndex even :: [Int] -> Bool))
    ]
