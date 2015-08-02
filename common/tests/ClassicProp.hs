module ClassicProp (tprops) where

import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
--import Test.QuickCheck.Property as Prop

import Data.Introhs.Classic

inEpsilon tolerance a b =
    --(a - delta) <= b && (a + delta) >= b
    not ((a + delta) < b) && not ((b + delta) < a)
  where delta = abs tolerance

epsilon = 0.001

--propFact :: (Eq a, Show a, Ord a, Num a, Enum a) => a -> Bool
propFact :: Word32 -> Bool
propFact n = foldl (\a f -> a && f n == ans) True [factLp, factI]
  where ans = foldl (*) 1 [1..n]

--propExpt :: (Eq a, Show a, Num a) => a -> a -> Bool
propExpt :: Float -> Float -> Bool
propExpt b n = foldl (\a f -> a && (inEpsilon (epsilon * ans) ans (f b n))) 
    True [exptLp, exptI]
  where ans = b ** n
    
-- Data.Functor.<$> ; GHC.Base.<*>   -- prefix fmap --> infix <$>
genTup2Int = (,) <$> (elements ([1..20] :: [Float])) <*>
    (elements ([2..10] :: [Float]))


tprops = map (uncurry testProperty) $ 
    [("fact n == List.product [1..n]", forAll (elements [0..18]) propFact)
    , ("expt b n == b ** n", (forAll genTup2Int . uncurry) propExpt)
    ]
