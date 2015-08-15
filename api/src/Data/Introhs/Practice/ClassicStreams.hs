module Data.Introhs.Practice.ClassicStreams where

import Data.List

squaresMap2, squaresU, squaresLc, squaresScanl :: [Float]
exptsMap2, exptsU, exptsLc, exptsScanl :: Float -> [Float]

sumsMap2, sumsU, sumsLc, sumsScanl :: Int -> [Int]
factsMap2, factsU, factsLc, factsScanl :: (Integral a) => [a]

fibsMap2, fibsU, fibsLc, fibsScanl :: [Int]
pascalrowsMap2, pascalrowsU, pascalrowsLc, pascalrowsScanl :: [[Int]]

squaresMap2 = 0.0 : 1.0 : zipWith (\_ e2 -> e2 * e2) squaresMap2 [2..]
exptsMap2 b = 1.0 : zipWith (\_ e2 -> b ** e2) (exptsMap2 b) [1..]

sumsMap2 lo = lo : zipWith (\e1 e2 -> e1 + e2 + lo) (sumsMap2 lo) [1..]
factsMap2 = 1 : zipWith (*) factsMap2 [1..]

fibsMap2 = 0 : 1 : zipWith (+) fibsMap2 (tail fibsMap2)
pascalrowsMap2 = [1] : zipWith (\e1 _ -> zipWith (+) (0 : e1) (e1 ++ [0])) 
    pascalrowsMap2 [0..]


squaresU = unfoldr (\(z, ct) -> Just (z, (ct * ct, ct + 1))) (0.0, 1)
exptsU b = unfoldr (\z -> Just (z, z * b)) 1.0

sumsU lo = unfoldr (\(z, ct) -> Just (z, (z + ct + lo, ct + 1))) (lo, 1)
factsU = unfoldr (\(z, ct) -> Just (z, (z * ct, ct + 1))) (1, 1)

fibsU = unfoldr (\(s0, s1) -> Just (s0, (s1, s0 + s1))) (0, 1)
pascalrowsU = unfoldr (\z -> Just (z, zipWith (+) (0 : z) (z ++ [0]))) [1]


squaresLc = [x * x | x <- [0.0, 1.0..]]
exptsLc b = [b ** x | x <- [0..]]

sumsLc lo = lo : [a + b + lo | (a, b) <- zip (sumsLc lo) [1..]]
factsLc = 1 : [a * b | (a, b) <- zip factsLc [1..]]

fibsLc = 0 : 1 : [a + b | (a, b) <- zip fibsLc (tail fibsLc)]
pascalrowsLc = [1] : [zipWith (+) (0 : row) (row ++ [0]) | 
    row <- pascalrowsLc]


squaresScanl = scanl (\_ e -> e * e) 0 [1..]
exptsScanl b = 1.0 : scanl (\a _ -> a * b) b (exptsScanl b)

sumsScanl lo = scanl (\a e -> a + e + lo) lo [1..]
factsScanl = scanl (*) 1 [1..]

fibsScanl = 0 : scanl (+) 1 fibsScanl
pascalrowsScanl = scanl (\_ row -> zipWith (+) (0 : row) (row ++ [0]))
    [1] pascalrowsScanl 
