
module Data.Introhs.Practice.ClassicPuzzles (module Data.Introhs.Practice.ClassicPuzzles) where

import qualified Text.Printf as Printf
--import qualified Data.Vector as V
import qualified Data.Map.Lazy as Map.Lazy
--import qualified Data.Array as A
import qualified Data.Char as Char
import Data.List
--import qualified Data.Maybe as Maybe

hanoi :: (Int, Int, Int) -> Int -> [(Int, Int)]
hanoi (src, dest, spare) numDisks
    | numDisks < 1 = []
    | otherwise = hanoi (src, spare, dest) (numDisks - 1) ++ 
        [(src, dest)] ++ hanoi (spare, dest, src) (numDisks - 1)

hanoiIO :: Int -> IO ()
hanoiIO numDisks = mapM_ f $ hanoi (1, 2, 3) numDisks
  where f (x, y) = putStrLn $ "move from " ++ show x ++ " to " ++ show y

hanoiMoves :: (Int, Int, Int) -> Int -> ([(Int, Int)], [String], [(String, [[Int]])])
hanoiMoves (src, dest, spare) numDisks = 
    (answer, [statTxt (length answer), replicate 40 '-'],
        zip (map proc answer) (hanoiPegs answer))
  where answer = hanoi (src, dest, spare) numDisks
        proc :: (Int, Int) -> String
        proc (h, t) = "" ++ Printf.printf "'move from %d to %d'" h t
        calcLen = truncate (2.0 ** fromIntegral numDisks :: Double) - 1
        statTxt resLen = 
            Printf.printf "(n = %d) 2^n - 1 = %d %s (length answer) = %d\n"
            numDisks calcLen
            (if calcLen == resLen then "==" :: String else "/=") resLen
        {-
        hanoiPegs res =                -- version using vectors
            reverse (iter (V.fromList [[1..numDisks], [], []]) res [])
          where iter vecPegs lst acc = case lst of
                    [] -> acc
                    (h, t):xs -> iter pegDnVec xs (V.toList pegDnVec : acc)
                      where (el1, el2) = (h - 1, t - 1)
                            lst2 = vecPegs V.! el2
                            pegDnVec = case vecPegs V.! el1 of
                                [] -> V.empty
                                d1:rst1 -> V.update (V.update vecPegs 
                                    (V.fromList [(el1, rst1)])) 
                                    (V.fromList [(el2, d1 : lst2)])
        -}{-
        hanoiPegs res =                -- version using alists
            reverse (iter [(0, [1..numDisks]), (1, []), (2, [])] res [])
          where iter lstPegs lst acc = case lst of
                    [] -> acc
                    (h, t):xs -> 
                        iter pegDnLst xs
                            ([Maybe.fromMaybe [] $ lookup 0 pegDnLst, 
                            Maybe.fromMaybe [] $ lookup 1 pegDnLst, 
                            Maybe.fromMaybe [] $ lookup 2 pegDnLst] : acc)
                      where (el1, el2) = (h - 1, t - 1)
                            lst2 = Maybe.fromMaybe [] $ lookup el2 lstPegs
                            pegDnLst = case Maybe.fromMaybe [] $
                                    lookup el1 lstPegs of
                                [] -> []
                                d1:rst1 -> (el2, d1:lst2) : ((el1, rst1) : 
                                    filter (\(k, _) -> k /= el1) lstPegs)
        -}
        hanoiPegs res =                -- version using dict/maps
            reverse (iter map1 res [])
          where map1 = Map.Lazy.fromList [(0, [1..numDisks]), (1, []), (2, [])]
                iter mapPegs lst acc = case lst of
                    [] -> acc
                    (h, t):xs -> 
                        iter pegDnMap xs (Map.Lazy.elems pegDnMap : acc)
                      where (el1, el2) = (h - 1, t - 1)
                            lst2 = Map.Lazy.findWithDefault [] el2 mapPegs
                            pegDnMap = case Map.Lazy.findWithDefault [] el1 
                                    mapPegs of
                                [] -> Map.Lazy.empty
                                d1:rst1 ->
                                    Map.Lazy.insert el2 (d1:lst2) $
                                    Map.Lazy.insert el1 rst1 mapPegs
        
nqueens :: Int -> [[(Int, Int)]]
nqueens n = iter 0 0 [] []
  where threatp (x1, y1) (x2, y2) = 
            (x1 == x2) || (y1 == y2) || (abs (x1 - x2) == abs (y1 - y2))
        safep pos placedSet = case placedSet of
            [] -> True
            x:xs -> not (threatp pos x) && safep pos xs
        iter col row placedSet board
            | (n - 1) < col = reverse placedSet : board
            | (n - 1) < row = board
            | safep (col, row) placedSet = 
                iter col (row + 1) placedSet (iter (col + 1) 0 
                    ((col, row) : placedSet) board)
            | otherwise = iter col (row + 1) placedSet board
{-
nqueensGrid :: Int -> [(Int, Int)] -> [String]
nqueensGrid numQueens answer =        -- version using 2d array
    --A.elems arr2d3
    [unwords [arr2d3 A.! (y,x) | x <- [0..numQueens]] | y <- [0..numQueens]]
    --[id [(arr2d3 A.! (y,x)) | x <- [0..numQueens]] | y <- [0..numQueens]]
  where lstN = [0..(numQueens - 1)]
        calcGrid el = abs ((el + 1) - numQueens)
        arr2d = A.listArray ((0, 0), (numQueens, numQueens)) $
            replicate ((numQueens + 1) * (numQueens + 1)) " "
        arr2d1 = foldl (\arr el -> arr A.// [((calcGrid el, 0), show el)]) 
            arr2d lstN
        arr2d2 = foldl (\arr el -> arr A.// [((numQueens, el + 1), 
            [Char.chr (el + Char.ord 'a')])]) arr2d1 lstN
        arr2d3 = foldl (\arr (h, t) -> arr A.// [((calcGrid t, h + 1), "Q")]) 
            arr2d2 answer
-}
nqueensGrid :: Int -> [(Int, Int)] -> [[String]]
nqueensGrid numQueens answer =        -- version using lists
    foldl mkRow [lstLtrs] (sortBy (\(_, at) (_, bt) -> compare at bt) 
        answer)
  where lstN = [0..(numQueens - 1)]
        lstBlank = zip lstN (map (const " ") lstN)
        lstLtrs = " " : map (\x -> [Char.chr (x + Char.ord 'a')]) lstN
        mkRow acc (h, t) = (show t : map (\(i, e) -> if i == h then "Q" 
            else e) lstBlank) : acc
