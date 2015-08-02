{-# LANGUAGE ScopedTypeVariables #-}
module NewProp (tprops) where

import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
--import Test.QuickCheck.Property as Prop
import Data.List

uncurry3 f3 (x, y, z) = f3 x y z

-- Data.Functor.<$> ; GHC.Base.<*>   -- prefix fmap --> infix <$>
genTup2 :: Gen a -> Gen a -> Gen (a, a)
genTup2 gen0 gen1 = (,) <$> gen0 <*> gen1

genTup3 :: Gen a -> Gen a -> Gen a -> Gen (a, a, a)
genTup3 gen0 gen1 gen2 = (,,) <$> gen0 <*> gen1 <*> gen2

--epsilon = 0.001
--(lst, revlst) = ([0..4], reverse (enumFromTo 0 4))

propCommutAdd :: (Eq a, Show a, Num a) => a -> a -> Bool
propCommutAdd x y = x + y == y + x

propAssocAdd :: (Eq a, Show a, Num a) => a -> a -> a -> Bool
propAssocAdd x y z = (x + y) + z == x + (y + z)

propRevRev, propRevId :: (Eq a, Show a) => [a] -> Bool
propRevRev xs = xs == (reverse . reverse) xs
propRevId xs = xs == reverse xs

propSortRev :: (Eq a, Show a, Ord a) => [a] -> Bool
propSortRev xs = sort xs == (sort . reverse) xs

propSortMin :: (Eq a, Show a, Ord a) => [a] -> Property
propSortMin xs = (not . null) xs ==> (head . sort) xs == minimum xs

propSortMinAppend:: (Eq a, Show a, Ord a) => NonEmptyList a -> NonEmptyList a -> Bool
propSortMinAppend (NonEmpty xs) (NonEmpty ys) = (head . sort) (xs ++ ys) == min (minimum xs) (minimum ys)


propCommutAddInts = (propCommutAdd :: Int -> Int -> Bool)
propAssocAddInts = (propAssocAdd :: Int -> Int -> Int -> Bool)

propCommutAddFlts = (propCommutAdd :: Float -> Float -> Bool)
propAssocAddFlts = (propAssocAdd :: Float -> Float -> Float -> Bool)

genC = (suchThat (elements [0..20]) even)
genTup2Int = genTup2 (elements [0..10]) (elements [(-18)..(-1)])
genTup3Int = genTup3 genC genC genC

propComb1 = (within 100) . forAll genTup2Int . uncurry
propComb2 = forAll genTup3Int . uncurry3
propComb3 = forAll (listOf1 $ elements [' '..'~'])


tprops = map (uncurry testProperty) $ 
    [("+ is commutAddInts", propComb1 propCommutAddInts)
    , ("+ is assocAddInts", propComb2 propAssocAddInts)
    , ("+ is commutAddFlts", property propCommutAddFlts)
    , ("+ is assocAddFlts", property propAssocAddFlts)
    , ("id == reverse . reverse", property (propRevRev :: [Int] -> Bool))
    , ("id == reverse", property (propRevId :: [Int] -> Bool))
    , ("sort == sort . reverse", propComb3 (propSortRev :: String -> Bool))
    , ("minimum == head . sort", property (propSortMin :: [Int] -> Property))
    , ("minimum == (head . sort) (++)", property (propSortMinAppend :: NonEmptyList Int -> NonEmptyList Int -> Bool))
    ]
