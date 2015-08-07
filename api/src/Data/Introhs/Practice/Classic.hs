
module Data.Introhs.Practice.Classic (module Data.Introhs.Practice.Classic
    , module Data.Introhs.Practice.ClassicPuzzles
    ) where

import Data.List
import qualified System.Log as Log
import qualified Control.Monad.Writer as Writer

import Data.Introhs.Practice.ClassicPuzzles

exptR, exptI, fastExptR, fastExptI :: Float -> Float -> Float
{-
exptR b n = if n == 0 then 1 else b * exptR b (n - 1)
-}
exptR b n | n == 0 = 1 | otherwise = b * exptR b (n - 1)

{-
exptI b n = iter 1 n
  where iter prod ct | ct == 0 = prod | otherwise = iter (prod * b) (ct - 1)
-}
exptI b n = 
    let iter prod ct | ct == 0 = prod | otherwise = iter (prod * b) (ct - 1)
    in iter 1 n

squareR, squareI :: Float -> Float
squareI n = exptI n 2.0 -- n * n  -- n ** 2.0
squareR n = exptR n 2.0

fastExptR b n
    | n == 0 = 1
    | even (truncate n :: Int) = fastExptR b (n / 2) ** 2.0
    | otherwise = b * fastExptR b (n - 1)

fastExptI b n = 
    let iter prod ct 
            | ct == 0 = prod
            | even ct = iter (prod * (b ** 2.0)) (ct - 2)
            | otherwise = iter (prod * b) (ct - 1)
    in iter 1.0 (truncate n :: Int)

numseqMathR, numseqMathI :: (Integral a) => a -> (a -> a -> a) -> a -> a -> a
{-
numseqMathR initial op hi lo
    | hi < lo = initial
    | otherwise = op hi (numseqMathR initial op (hi - 1) lo)
-}
numseqMathR initial op hi lo
    | hi < lo = initial
    | otherwise = op (numseqMathR initial op hi (lo + 1)) lo

numseqMathI initial op hi lo =
    let iter start acc
            | hi < start = acc
            | otherwise = iter (start + 1) (op acc start)
    in iter lo initial

sumToR, sumToI :: Int -> Int -> Int
sumToR hi lo = numseqMathR lo (+) hi (lo + 1)
sumToI hi lo = numseqMathI lo (+) hi (lo + 1)

factR, factI :: (Integral a) => a -> a
factR n = numseqMathR 1 (*) n 1
factI n = numseqMathI 1 (*) n 1

fibR, fibI :: Int -> Int
fibR n
    | n < 2 = n 
    | otherwise = fibR (n - 2) + fibR (n - 1)

fibI n =
    let iter sum0 sum1 ct
            | ct == 0 = sum0
            | otherwise = iter sum1 (sum0 + sum1) (ct - 1)
    in iter 0 1 n

pascaltriMult, pascaltriAdd :: Int -> [[Int]]
pascaltriMult rows = map pascalrow (enumFromTo 1 (rows + 1))
  where pascalrow r = iter 1 [1]
          where iter col xs
                    | r == col = xs
                    | otherwise = (case xs of
                            [] -> error "empty list"
                            y:_ -> iter (col + 1) (truncate (fromIntegral y *
                                fromIntegral (r - col) / 
                                fromIntegral col :: Double) : xs))

pascaltriAdd rows = triangle [1] (rows + 1)
  where nextRow xs = zipWith (+) (0 : xs) (xs ++ [0])
        triangle xs numRows
            | numRows == 0 = []
            | otherwise = xs : triangle (nextRow xs) (numRows - 1)

quotMremM :: Int -> Int -> (Int, Int)
quotMremM a b = (q, a - (q * b))
  where q = truncate $ fromIntegral a / (fromIntegral b :: Double)

quotM, remM :: Int -> Int -> Int
quotM a b = fst $ quotMremM a b
remM a b = snd $ quotMremM a b

divMmodM :: Int -> Int -> (Int, Int)
divMmodM a b = (qFloor, a - (qFloor * b))
  where qFloor = floor $ fromIntegral a / (fromIntegral b :: Double)

divM, modM :: Int -> Int -> Int
divM a b = fst $ divMmodM a b
modM a b = snd $ divMmodM a b

euclidR, euclidI :: Int -> Int -> Int
euclidR m n | n == 0 = abs m | otherwise = euclidR n (mod m n)

euclidI m n =
    let iter a b | b == 0 = abs a | otherwise = iter b (mod a b)
    in iter m n

gcdR, gcdI :: [Int] -> Int
gcdR xs = case xs of
    [] -> 0
    [m] -> abs m
    m:n:rst -> gcdR (euclidR m n : rst)

gcdI [] = 0
gcdI (x:xs) = 
    let iter acc rst = case rst of
            [] -> abs acc
            m:ns -> iter (euclidI acc m) ns
    in iter x xs

lcmR, lcmI :: [Int] -> Int
lcmR xs = case xs of
    [] -> 0
    [m] -> abs m
    m:n:rst -> lcmR ((m * floor (fromIntegral n / 
        fromIntegral (euclidR m n) :: Double)) : rst)

lcmI [] = 0
lcmI (x:xs) = 
    let iter acc rst = case rst of
            [] -> abs acc
            m:ns -> iter (acc * floor (fromIntegral m / 
                fromIntegral (euclidI acc m) :: Double)) ns
    in iter x xs

baseExpandR, baseExpandI :: Int -> Int -> [Int]
baseExpandR b n
    | n == 0 = []
    | otherwise = baseExpandR b (quot n b) ++ [mod n b]

baseExpandI b n = 
    let iter q xs
            | q == 0 = xs
            | otherwise = iter (quot q b) (mod q b : xs)
    in iter n []

baseTo10R, baseTo10I :: Int -> [Int] -> Int
baseTo10R b xs = case xs of
    [] -> 0
    n:ns -> baseTo10R b ns + (n * floor (fromIntegral b ** 
        fromIntegral (length ns) :: Double))

baseTo10I b xs = 
    let iter ys acc ct = case ys of
            [] -> acc
            n:ns -> iter ns (acc + (n * floor (fromIntegral b ** 
                fromIntegral ct :: Double))) (ct + 1)
    in iter (reverse xs) (0 :: Int) (0 :: Int)

rangeStepR, rangeStepI :: Int -> Int -> Int -> [Int]
rangeStepR step start stop
    | cmpOp start stop = []
    | otherwise = start : rangeStepR step (start + step) stop
  where cmpOp | step > 0 = (>) | otherwise = (<)

rangeStepI step start stop = reverse $ iter start []
  where cmpOp | step > 0 = (>) | otherwise = (<)
        iter cur acc 
            | cmpOp cur stop = acc 
            | otherwise = iter (cur + step) (cur : acc)

rangeR, rangeI :: Int -> Int -> [Int]
rangeR start stop = rangeStepR 1 start stop
rangeI start stop = rangeStepI 1 start stop

compose1 :: (b -> c) -> (a -> b) -> a -> c
compose1 f g x = f (g x)


factILog :: Integer -> Writer.Writer [(Log.Priority, String)] Integer
factILog n = do
    Writer.tell [(Log.INFO, "factI()")]
    return $ factI n


libmain :: IO ()
libmain = do
    putStrLn $ "factI 5: " ++ (show $ factI 5)
