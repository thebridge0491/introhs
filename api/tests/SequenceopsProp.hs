{-# LANGUAGE ScopedTypeVariables #-}
module SequenceopsProp (tprops) where

import Data.List
import Data.Word
import qualified Control.Monad.Writer as Writer

import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
--import Test.QuickCheck.Property as Prop

import qualified Data.Introhs.Util as Util
import Data.Introhs.Practice.Sequenceops

--epsilon = 0.001

propTabulate :: Int -> Property
propTabulate n = (n > 0 && n <= 10) ==> 
    foldl (\a f -> a && f (\e -> e) n == ans && f (\e -> e + 2) n == ans2) True
    [tabulateR, tabulateI]
  where ans = reverse $ foldl (\a e -> e : a) [] [0..(n - 1)]
        ans2 = reverse $ foldl (\a e -> e : a) [] [2..(n - 1 + 2)]

propLength :: (Eq a, Show a) => [a] -> Bool
propLength xs = foldl (\a f -> a && f xs == ans) True 
    [lengthR, lengthI]
  where ans = length xs

propNth :: (Eq a, Show a) => Int -> [a] -> Property
propNth idx xs = (idx >= 0 && idx < length xs) ==> 
    foldl (\a f -> a && f idx xs == Just ans) True 
    [nthR, nthI]
  where ans = xs !! idx

propFindIndex :: (Eq a, Show a) => (a -> Bool) -> [a] -> Bool
propFindIndex pred1 xs = foldl (\a f -> a && f pred1 xs == ans) True 
    [findIndexR, findIndexI]
  where ans = findIndex pred1 xs

propFind pred1 xs = foldl (\a f -> a && f pred1 xs == ans) True 
    [findR, findI]
  where ans = find pred1 xs

propMinMax :: (Eq a, Show a, Ord a) => [a] -> Property
propMinMax xs = (not . null) xs ==> 
    foldl (\a (fMin, fMax) -> a && fMin xs == ansMin 
        && fMax xs == ansMax) True
        [(minR, maxR), (minI, maxI)]
  where (ansMin, ansMax) = (minimum xs, maximum xs)

propReverse :: (Eq a, Show a) => [a] -> Bool
propReverse xs = foldl (\a f -> a && f xs == ans) True 
    [reverseR, reverseI]
  where ans = reverse xs

propCopy :: (Eq a, Show a) => [a] -> Bool
propCopy xs = foldl (\a f -> a && f xs == ans) True 
    [copyR, copyI]
  where ans = map id xs

propTakeDrop :: (Eq a, Show a) => Int -> [a] -> Property
propTakeDrop n xs = (n >= 0 && n < 50) ==> foldl (\a (fT, fD) -> a && 
    fT n xs == ansT && fD n xs == ansD) True 
    [(takeI, dropI)]
  where (ansT, ansD) = (take n xs, drop n xs)

propAnyAll :: (Eq a, Show a) => (a -> Bool) -> [a] -> Property
propAnyAll pred1 xs = (length xs < 50) ==> foldl (\a (fAny, fAll) -> a && 
    fAny pred1 xs == ansAny && fAll pred1 xs == ansAll) True 
    [(anyR, allR), (anyI, allI)]
  where (ansAny, ansAll) = (any pred1 xs, all pred1 xs)

propMap :: (Eq b, Show b) => (a -> b) -> [a] -> Bool
propMap proc xs = foldl (\a f -> a && f proc xs == ans) True 
    [mapR, mapI]
  where ans = map proc xs

propFilterRemove :: (Eq a, Show a) => (a -> Bool) -> [a] -> Bool
propFilterRemove pred1 xs = foldl (\a (fFil, fRem) -> a && 
    fFil pred1 xs == ansFil && fRem pred1 xs == ansRem) True 
    [(filterR, removeR), (filterI, removeI)]
  where (ansFil, ansRem) = (filter pred1 xs, snd $ partition pred1 xs)

propFoldl :: (Eq b, Show b) => (b -> a -> b) -> b -> [a] -> Property
propFoldl corp init xs = (length xs < 50) ==> 
    foldl (\a f -> a && f corp init xs == ans) True 
    [foldlR, foldlI]
  where ans = foldl corp init xs

propFoldr :: (Eq b, Show b) => (a -> b -> b) -> b -> [a] -> Property
propFoldr proc init xs = (length xs < 50) ==> 
    foldl (\a f -> a && f proc init xs == ans) True 
    [foldrR, foldrI]
  where ans = foldr proc init xs

propUnfoldleft :: (Eq a, Eq b, Show a, Show b) => (b -> Maybe (a, b)) -> b -> Bool
propUnfoldleft func seed = foldl (\a f -> a && f func seed == ans) True 
    [(unfoldleftR)]
  where ans = unfoldr func seed

propUnfoldright :: (Eq a, Eq b, Show a, Show b) => (b -> Maybe (a, b)) -> b -> Bool
propUnfoldright func seed = foldl (\a f -> a && f func seed == ans) True 
    [(unfoldrightI)]
  where ans = reverse $ unfoldr func seed

propIsOrdered :: (Ord a, Show a) => [a] -> Property
propIsOrdered xs = (length xs < 25) ==> 
    foldl (\a f -> a && f xs False == ans && f xs True == ans2) True 
    [isOrderedR, isOrderedI]
  where verifyfn _ [] = True
        verifyfn cmpfn (y:ys) = 
            fst $ foldl (\(a, cur) e -> ((cmpfn cur e) && a, e)) (True, y) ys
        ans = (verifyfn (<=) xs)
        ans2 = (verifyfn (>=) xs)

propAppend :: (Eq a, Show a) => [a] -> [a] -> Bool
propAppend xs ys = foldl (\a f -> a && f xs ys == ans) True 
    [appendR, appendI]
  where ans = xs ++ ys

propInterleave :: (Eq a, Show a) => [a] -> [a] -> Property
propInterleave xss yss = (length xss < 50) ==> (length yss < 50) ==>
    foldl (\a f -> a && f xss yss == ans) True
    [interleaveR, interleaveI]
  where ans 
            | length xss > length yss = 
                concat (zipWith (\x y -> [x, y]) xss yss) ++ 
                drop (length yss) xss
            | otherwise = concat (zipWith (\x y -> [x, y]) xss yss) ++ 
                drop (length xss) yss

propMap2 :: (Eq c, Show c) => (a -> b -> c) -> [a] -> [b] -> Property
propMap2 proc xs ys = (length xs < 25) ==> (length ys < 25) ==> 
    foldl (\a f -> a && f proc xs ys == ans) True 
    [map2R, map2I]
  where ans = zipWith proc xs ys

propZip :: (Eq a, Eq b, Show a, Show b) => [a] -> [b] -> Property
propZip xs ys = (length xs < 25) ==> (length ys < 25) ==> 
    foldl (\a f -> a && f xs ys == ans) True 
    [zipR, zipI, zipM]
  where ans = zip xs ys

propUnzip2 :: (Eq a, Eq b, Show a, Show b) => [(a, b)] -> Property
propUnzip2 xs = (length xs < 25) ==> 
    foldl (\a f -> a && f xs == ans) True 
    [unzip2I]
  where ans = unzip xs

propConcat :: (Eq a, Show a) => [[a]] -> Property
propConcat nlsts = (length nlsts < 20) ==> 
    foldl (\a f -> a && f nlsts == ans) True 
    [concatR, concatI]
  where ans = concat nlsts


propReverseLog :: (Eq a, Show a) => [a] -> Bool
propReverseLog xs = foldl (\a f -> do
    let (lstRev, logReverseI) = Writer.runWriter $ f xs
    a && lstRev == ans && foldl (\a1 (_, msg) -> a1 && not (null msg))
        True logReverseI) True
    [reverseILog]
  where ans = reverse xs


-- Data.Functor.<$> ; GHC.Base.<*>   -- prefix fmap --> infix <$>
genTup2 xs ys = (,) <$> elements xs <*> elements ys
--genTup2Int = (,) <$> (elements ([1..20] :: [Float])) <*>
--    (elements ([2..10] :: [Float]))

--propFindIndexEven = propFindIndex even :: [Int] -> Bool
--propFind3 = propFind (== 3) :: [Int] -> Bool
--propFilterRemoveEven = propFilterRemove even :: [Int] -> Bool
--propAnyAllEven = propAnyAll even :: [Int] -> Property
--propFoldlAdd = propFoldl (+) :: Int -> [Int] -> Property
--propFoldrSub = propFoldr (-) :: Int -> [Int] -> Property

--propMapPlus2 = propMap (+2) :: [Int] -> Bool
--propMap2Plus2 = propMap2 (\e1 e2 -> (e1 + e2) + 2) :: [Int] -> [Int] -> Property

propUnfoldrightRange n = propUnfoldright funcRange (0, n)
  where funcRange (lo, hi) = if lo > hi then Nothing 
            else Just (lo, (lo + 1, hi))
propUnfoldleftFib n = propUnfoldleft funcFib (0, 1, n)
  where funcFib (s0, s1, num) = if num < 0 then Nothing 
            else Just (s0, (s0 + s1, s0, num - 1))
propUnfoldrightBaseExpand b = propUnfoldright funcBsExpand
  where funcBsExpand num = if num <= 0 then Nothing 
            else Just (mod num b, div num b)
propUnfoldleftUnsum n = propUnfoldleft funcUnfoldSum (0, n)
  where funcUnfoldSum (start, stop) = if start > stop then Nothing 
            else Just (start, (start + 1, stop - start))
propUnfoldrightUnprod n = propUnfoldright funcUnfoldProd (1, n)
  where funcUnfoldProd (start, stop) = if start > stop then Nothing 
            --else Just (start, (start + 1, stop / start))
            else Just (start, (start * stop, stop - 1))

propComb0 = forAll (elements $ map (\n -> sum [0..n]) [0..25])
propComb1 = forAll (elements $ map (\n -> product [1..n]) [1..17])
propComb2 = forAll (genTup2 [2..16] [0..1024]) . uncurry


tprops = map (uncurry testProperty) $ 
    [("tabulate == reverse $ foldl (\a e -> e : a) [] [0..(n - 1)]"
        , property (propTabulate :: Int -> Property))
    , ("length == List.length", property (propLength :: [Int] -> Bool))
    , ("nth == List.!!", property (propNth :: Int -> [Int] -> Property))
    , ("findIndex even == List.findIndex", property (propFindIndex even :: [Int] -> Bool))
    , ("find (== 3) == List.find (== 3)", property (propFind (== 3) :: [Int] -> Bool))
    , ("minMax == List.[min|max]imum"
        , property (propMinMax :: [Int] -> Property))
    , ("reverse == List.reverse", property (propReverse :: [Int] -> Bool))
    , ("copy == List.map id", property (propCopy :: [Int] -> Bool))
    , ("[take|drop] n == List.[take|drop] n"
        , property (propTakeDrop :: Int -> [Int] -> Property))
    , ("[any|all] even == List.[any|all] even", property (propAnyAll even :: [Int] -> Property))
    , ("map (+2) == List.map (+2)", property (propMap (+2) :: [Int] -> Bool))
    , ("[filter|remove] even == List.[filter|partition] even"
        , property (propFilterRemove even :: [Int] -> Bool))
    , ("foldl (+) == List.foldl (+)", property (propFoldl (+) :: Int -> [Int] -> Property))
    , ("foldr (-) == List.foldr (-)", property (propFoldr (-) :: Int -> [Int] -> Property))
    , ("unfoldright funcRange (0, n) == List.unfoldr funcRange (0, n)"
        , forAll (elements [0..25]) propUnfoldrightRange)
    , ("unfoldleft funcFib (0, 1, n) == reverse $ List.unfoldr funcFib (0, 1, n)"
        , forAll (elements [0..25]) propUnfoldleftFib)
    , ("unfoldright funcBsExpand (b, n) == List.unfoldr funcBsExpand (b, n)"
        , propComb2 propUnfoldrightBaseExpand)
    , ("unfoldleft funcUnfoldSum (0, n) == reverse $ List.unfoldr funcUnfoldSum (0, n)"
        , propComb0 propUnfoldleftUnsum)
    , ("unfoldright funcUnfoldProd (1, n) == List.unfoldr funcUnfoldProd (1, n)"
        , propComb1 propUnfoldrightUnprod)
    , ("isOrderedInts == fst $ foldl (\a e -> ((x <= e) && a, e)) True xs"
        , property (propIsOrdered :: [Int] -> Property))
    , ("isOrderedChars == fst $ foldl (\a e -> ((x <= e) && a, e)) True xs"
        , property (propIsOrdered :: [Char] -> Property))
    , ("append == List.++"
        , property (propAppend :: [Int] -> [Int] -> Bool))
    , ("interleave xs ys == ?? xs ys"
        , property (propInterleave :: [Int] -> [Int] -> Property))
    , ("map2 (e1 + e2 + 2) == List.zipWith (e1 + e2 +2)"
        , property (propMap2 (\e1 e2 -> (e1 + e2) + 2) :: [Int] -> [Int] -> Property))
    , ("zip == List.zip", property (propZip :: [Int] -> [Int] -> Property))
    , ("unzip2 == List.unzip"
        , property (propUnzip2 :: [(Int, Int)] -> Property))
    , ("concat == List.concat", property (propConcat :: [[Int]] -> Property))
    
    , ("reverseLog == List.reverse"
        , property (propReverseLog :: [Int] -> Bool))
  ]
