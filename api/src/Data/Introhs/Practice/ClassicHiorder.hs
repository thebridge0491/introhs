module Data.Introhs.Practice.ClassicHiorder where

import Data.List
--import qualified Data.Maybe as Maybe

import qualified Data.Introhs.Util as Util

exptF :: Float -> Float -> Float
exptF b n = foldl (\a _ -> a * b) 1.0 [1.0..n]

squareF :: Float -> Float
squareF n = exptF n 2.0

numseqMathF :: (Integral a) => a -> (a -> a -> a) -> a -> a -> a
numseqMathF initial op hi lo =
    foldl (\a e -> op a e) initial [lo..hi]

sumToF :: Int -> Int -> Int
sumToF hi lo = numseqMathF lo (+) hi (lo + 1)

factF :: (Integral a) => a -> a
factF n = numseqMathF 1 (*) n 1

fibF :: Int -> Int
fibF n = snd $ foldl (\(s0, s1) _ -> (s0 + s1, s0)) (0, 1) [0..n]

pascaltriF :: Int -> [[Int]]
pascaltriF rows = reverse $ 
    foldl (\(x:xs) _ -> zipWith (+) (0 : x) (x ++ [0]) : (x:xs))
    [[1]] [1..rows]

gcdF, lcmF :: [Int] -> Int
gcdF [] = 0
gcdF (m:ms) = foldl gcd (abs m) ms

lcmF [] = 0
lcmF (m:ms) = foldl lcm (abs m) ms

baseExpandF :: Int -> Int -> [Int]
baseExpandF b n =
    fst $ foldl (\(acc, num) _ -> case 0 == num of
            True -> (acc, quot num b)
            False -> ((mod num b) : acc, quot num b))
        ([], n) [0..(truncate $ logBase (fromIntegral b) (fromIntegral n))]

baseTo10F :: Int -> [Int] -> Int
baseTo10F b xs = snd $ foldr proc (0 :: Int, 0) xs
  where proc e (h, t) = (h + 1, t + (e * truncate (fromIntegral b ** 
            (fromIntegral h :: Double))))

rangeStepF :: Int -> Int -> Int -> [Int]
rangeStepF step start stop = 
    reverse $ foldl (\acc e -> case cmpOp e stop of
            True -> acc
            False -> e : acc)
        [] [start, (start + step)..stop]
  where cmpOp | step > 0 = (>) | otherwise = (<)

rangeF :: Int -> Int -> [Int]
rangeF start stop = rangeStepF 1 start stop


exptU :: Float -> Float -> Float
exptU b n = Util.headOr 1.0 $ reverse $ unfoldr funcExpt (1, b, n)
  where funcExpt (a, bs, ct) | ct < 0 = Nothing
            | otherwise = Just (a, (a * bs, bs, ct - 1))

squareU :: Float -> Float
squareU n = exptU n 2.0

numseqMathU :: (Integral a) => a -> (a -> a -> a) -> a -> a -> a
numseqMathU initial op hi lo = Util.headOr initial $ reverse $
    unfoldr func (initial, lo, hi)
  where func (a, n0, n1) | n0 > n1 = Nothing
            | otherwise = Just (op a n0, (op a n0, n0 + 1, n1))

sumToU :: Int -> Int -> Int
sumToU hi lo = numseqMathU lo (+) hi (lo + 1)

factU :: (Integral a) => a -> a
factU n = numseqMathU 1 (*) n 1

fibU :: Int -> Int
fibU n = Util.headOr n $ reverse $ unfoldr funcFib (0, 1, n)
  where funcFib (s0, s1, num) | num <= 0 = Nothing
            | otherwise = Just (s1, (s1, s0 + s1, num - 1))

pascaltriU :: Int -> [[Int]]
pascaltriU rows = unfoldr funcPascal ([[1]], rows)
  where funcPascal ([], _) = error "empty list"
        funcPascal (x:xs, ct) | ct < 0 = Nothing
            | otherwise = Just (x, (zipWith (+) (0 : x) (x ++ [0]) : (x:xs),
                ct - 1))

{-
euclidU :: Int -> Int -> Int
euclidU m n = Util.headOr (abs m) $ reverse $ unfoldr funcEuclid (m, n)
  where funcEuclid (h, t) | t == 0 = Nothing
            | otherwise = Just (t, (t, mod h t))
-}

gcdU, lcmU :: [Int] -> Int
gcdU [] = 0
gcdU (m:ms) = Util.headOr (abs m) $ reverse $ unfoldr func (m, ms)
  where func (a, rst) = case rst of
            [] -> Nothing
            b:bs -> Just (gcd a b, (gcd a b, bs))

lcmU [] = 0
lcmU (m:ms) = Util.headOr (abs m) $ reverse $ unfoldr func (m, ms)
  where func (a, rst) = case rst of
            [] -> Nothing
            b:bs -> Just (lcm a b, (lcm a b, bs))

baseExpandU :: Int -> Int -> [Int]
baseExpandU b n = reverse $ unfoldr funcBsExpand (b, n)
  where funcBsExpand (bs, num) | num <= 0 = Nothing
            | otherwise = Just (mod num bs, (bs, div num bs))

baseTo10U :: Int -> [Int] -> Int
baseTo10U b xss = Util.headOr 0 $ reverse $ unfoldr funcBase10 (0, b, xss)
  where genBase10 _ _ [] = 0
        genBase10 a bs (x:xs) = a + (x * truncate
                (fromIntegral bs ** fromIntegral (length xs) :: Double))
        funcBase10 (_, _, []) = Nothing
        funcBase10 (a, bs, x:xs) = Just (genBase10 a (bs :: Int) (x:xs), 
            (genBase10 a bs (x:xs), bs, xs))

rangeStepU :: Int -> Int -> Int -> [Int]
rangeStepU step start stop = unfoldr funcRange start
  where cmpOp | step > 0 = (>) | otherwise = (<)
        funcRange cur | cmpOp cur stop = Nothing 
            | otherwise = Just (cur, cur + step)

rangeU :: Int -> Int -> [Int]
rangeU start stop = rangeStepU 1 start stop


exptLc :: Float -> Float -> Float
--exptLc b n = product [b | _ <- [1..n]]
exptLc b n = Util.headOr 1 $ reverse [b ** x | x <- [0..n]]

squareLc :: Float -> Float
squareLc n = exptLc n 2.0

numseqMathLc :: (Integral a) => a -> (a -> a -> a) -> a -> a -> a
numseqMathLc initial op hi lo =
    let _helperLc =
            initial : [op a b | (a, b) <- zip _helperLc [lo..hi]]
    in (reverse _helperLc) !! 0

sumToLc :: Int -> Int -> Int
--sumToLc hi lo = sum $ lo : [x | x <- [(lo+1)..hi]]
sumToLc hi lo = numseqMathLc lo (+) hi (lo+1)

factLc :: (Integral a) => a -> a
--factLc n = product [x | x <- [1..n]]
factLc n = numseqMathLc 1 (*) n 1

fibLc :: Int -> Int
fibLc n =
    let _helperLc 0 = [0]
        _helperLc m =
            0 : 1 : [a + b | (_, a, b) <- zip3 [2..n] (_helperLc m)
                (tail $ _helperLc m)]
    in (reverse $ _helperLc n) !! 0

pascaltriLc :: Int -> [[Int]]
pascaltriLc rows =
    [1] : [zipWith (+) (0 : row) (row ++ [0]) | 
        (_, row) <- zip [1..rows] (pascaltriLc rows)]

baseExpandLc :: Int -> Int -> [Int]
baseExpandLc b n =
    [mod m b | m <- 
        reverse [quot n (truncate $ fromIntegral b ** fromIntegral i) |
        i <- [0..(truncate $ logBase (fromIntegral b) (fromIntegral n))]]]

baseTo10Lc :: Int -> [Int] -> Int
baseTo10Lc b xss =
    sum [x * (truncate $ fromIntegral b ** fromIntegral i) |
        (i, x) <- zip [0..(length xss)] (reverse xss)]
