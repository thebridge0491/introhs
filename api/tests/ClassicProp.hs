{-# LANGUAGE ScopedTypeVariables #-}
module ClassicProp (tprops) where

import Data.Word

import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
--import Test.QuickCheck.Property as Prop

import qualified Data.Introhs.Util as Util
import Data.Introhs.Practice.Classic

epsilon = 0.001

--propFact :: (Eq a, Show a, Ord a, Num a, Enum a) => a -> Bool
propFact :: Word32 -> Bool
propFact n = foldl (\a f -> a && f n == ans) True [factR, factI]
  where ans = foldl (*) 1 [1..n]

--propExpt :: (Eq a, Show a, Num a) => a -> a -> Bool
propExpt :: Float -> Float -> Bool
propExpt b n = foldl (\a f -> a && (Util.inEpsilon (epsilon * ans) ans (f b n))) 
    True [exptR, exptI]
  where ans = b ** n
    
-- Data.Functor.<$> ; GHC.Base.<*>   -- prefix fmap --> infix <$>
genTup2Int = (,) <$> (elements ([1..20] :: [Float])) <*>
    (elements ([2..10] :: [Float]))


tprops = map (uncurry testProperty) $ 
    [("fact n == List.product [1..n]", forAll (elements [0..18]) propFact)
    , ("expt b n == b ** n", (forAll genTup2Int . uncurry) propExpt)
    ]
