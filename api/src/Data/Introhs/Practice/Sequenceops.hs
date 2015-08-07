
module Data.Introhs.Practice.Sequenceops (module Data.Introhs.Practice.Sequenceops) where

import qualified System.Log as Log
import qualified Control.Monad.Writer as Writer

tabulateR, tabulateI :: (Int -> a) -> Int -> [a]
tabulateR func cnt
    | 1 > cnt = []
    | otherwise = (tabulateR func (cnt - 1)) ++ [func (cnt - 1)]

tabulateI func cnt =
    let iter idx acc
            | 1 > idx = acc
            | otherwise = iter (idx - 1) (func (idx - 1) : acc)
    in iter cnt []

lengthR, lengthI :: [a] -> Int
lengthR [] = 0
lengthR (_:xs) = 1 + lengthR xs

lengthI xs = iter 0 xs
  where iter acc [] = acc
        iter acc (_:ys) = iter (acc + 1) ys

nthR, nthI :: Int -> [a] -> Maybe a
nthR _ [] = Nothing
nthR idx (x:xs) | idx == 0 = Just x | otherwise = nthR (idx - 1) xs

nthI idx xs = iter idx xs
  where iter _ [] = Nothing
        iter ndx (y:ys) | ndx == 0 = Just y | otherwise = iter (ndx - 1) ys

indexFindR, indexFindI :: Int -> (a -> Bool) -> [a] -> (Maybe Int, Maybe a)
indexFindR _ _ [] = (Nothing, Nothing)
indexFindR ndx pred1 (x:xs)
    | pred1 x = (Just ndx, Just x)
    | otherwise = indexFindR (ndx + 1) pred1 xs

indexFindI ndx pred1 xs = iter ndx xs
    where iter _ [] = (Nothing, Nothing)
          iter idx (y:ys) 
                | pred1 y = (Just idx, Just y)
                | otherwise = iter (idx + 1) ys

findIndexR, findIndexI :: (a -> Bool) -> [a] -> Maybe Int
findR, findI :: (a -> Bool) -> [a] -> Maybe a

findIndexR pred1 xs = fst $ indexFindR 0 pred1 xs
findIndexI pred1 xs = fst $ indexFindI 0 pred1 xs

findR pred1 xs = snd $ indexFindR 0 pred1 xs
findI pred1 xs = snd $ indexFindI 0 pred1 xs

minmaxR, minmaxI :: (Ord a) => [a] -> (a, a)
minmaxR [] = error "empty list"
minmaxR xss = 
    let _helperR norm (x:xs) = case xs of
            [] -> x
            y:ys -> if norm (x < y) then _helperR norm $ x:ys
                else _helperR norm $ y:ys
    in (_helperR id xss, _helperR not xss)

minmaxI [] = error "empty list"
minmaxI (x:xs) = iter (x, x) xs
  where iter (lo, hi) [] = (lo, hi)
        iter (lo, hi) (y:ys)
            | y < lo = iter (y, hi) ys
            | y > hi = iter (lo, y) ys
            | otherwise = iter (lo, hi) ys

minR, maxR, minI, maxI :: (Ord a) => [a] -> a
minR xs = (fst . minmaxR) xs
maxR xs = (snd . minmaxR) xs
minI xs = (fst . minmaxI) xs
maxI xs = (snd . minmaxI) xs

reverseR, reverseI, copyR, copyI :: [a] -> [a]
reverseR [] = []
reverseR (x:xs) = reverseR xs ++ [x]

reverseI xs = iter xs []
  where iter [] acc = acc
        iter (y:ys) acc = iter ys (y : acc)

copyR [] = []
copyR (x:xs) = x : copyR xs

copyI xs = iter (reverse xs) []
  where iter [] acc = acc
        iter (y:ys) acc = iter ys (y : acc)

splitAtI :: Int -> [a] -> ([a], [a])
splitAtI n xs = iter n xs []
  where iter _ [] acc = (reverse acc, [])
        iter m (y:ys) acc
            | m == 0 = (reverse acc, y:ys)
            | otherwise = iter (m - 1) ys (y : acc)

takeI, dropI :: Int -> [a] -> [a]
takeI n xs = fst $ splitAtI n xs
dropI n xs = snd $ splitAtI n xs

anyAllR, anyAllI:: (a -> Bool) -> [a] -> (Bool, Bool)
anyAllR _ [] = (False, True)
anyAllR pred1 (x:xs) = (pred1 x || (fst $ anyAllR pred1 xs), 
    pred1 x && (snd $ anyAllR pred1 xs))

anyAllI pred1 xs = iter (False, True) xs
  where iter (a0, a1) [] = (a0, a1)
        iter (a0, a1) (y:ys) = iter (a0 || pred1 y, a1 && pred1 y) ys

anyR, allR, anyI, allI :: (a -> Bool) -> [a] -> Bool
anyR pred1 xs = fst $ anyAllR pred1 xs
anyI pred1 xs = fst $ anyAllI pred1 xs

allR pred1 xs = snd $ anyAllR pred1 xs
allI pred1 xs = snd $ anyAllI pred1 xs

mapR, mapI :: (a -> b) -> [a] -> [b]
mapR _ [] = []
mapR proc (x:xs) = proc x : mapR proc xs

mapI proc xs = iter (reverse xs) []
  where iter [] acc = acc
        iter (y:ys) acc = iter ys (proc y : acc)

mapM_R, mapM_I :: Monad m => (a -> m b) -> [a] -> m ()
mapM_R _ [] = return ()
mapM_R proc (x:xs) = do {_ <- proc x ; mapM_R proc xs}

mapM_I proc xs = iter xs
  where iter [] = return ()
        iter (y:ys) = do {_ <- proc y ; iter ys}

partitionR, partitionI :: (a -> Bool) -> [a] -> ([a], [a])
partitionR _ [] = ([], [])
partitionR pred1 xs = 
    let _helperR norm rst = case rst of
            [] -> []
            y:ys -> if norm (pred1 y) then y : _helperR norm ys
                else _helperR norm ys
    in (_helperR id xs, _helperR not xs)

partitionI pred1 xs = iter (reverse xs) ([], [])
  where iter [] acc = acc
        iter (y:ys) acc
            | pred1 y = iter ys (y : fst acc, snd acc)
            | otherwise = iter ys (fst acc, y : snd acc)

filterR, removeR, filterI, removeI :: (a -> Bool) -> [a] -> [a]
filterR pred1 xs = fst $ partitionR pred1 xs
removeR pred1 xs = snd $ partitionR pred1 xs
filterI pred1 xs = fst $ partitionI pred1 xs
removeI pred1 xs = snd $ partitionI pred1 xs

foldlR, foldlI :: (b -> a -> b) -> b -> [a] -> b
foldlR _ acc [] = acc
foldlR corp acc (x:xs) = foldlR corp (corp acc x) xs

foldlI corp acc0 xs = iter acc0 xs
  where iter acc [] = acc
        iter acc (y:ys) = iter (corp acc y) ys

foldrR, foldrI :: (a -> b -> b) -> b -> [a] -> b
foldrR _ acc [] = acc
foldrR proc acc (x:xs) = proc x (foldrR proc acc xs)

foldrI proc acc0 xs = iter (reverse xs) acc0
  where iter [] acc = acc
        iter (y:ys) acc = iter ys (proc y acc)

unfoldleftR, unfoldrightI :: (b -> Maybe (a, b)) -> b -> [a]
unfoldleftR func seed = case func seed of
    Nothing -> []
    Just (a, newSeed) -> a : unfoldleftR func newSeed

unfoldrightI func seed = 
    let iter cur acc = case func cur of
            Nothing -> acc
            Just (a, newCur) -> iter newCur (a : acc)
    in iter seed []

isOrderedR, isOrderedI :: (Ord a) => [a] -> Bool -> Bool
isOrderedR xs isRev = case xs of
    [] -> True
    _:[] -> True
    y:z:zs -> case isRev of
                False -> (y <= z) && (isOrderedR (z:zs) isRev)
                True -> (y >= z) && (isOrderedR (z:zs) isRev)

isOrderedI [] _ = True
isOrderedI (x:xs) isRev = 
    let iter rst oldval acc = case rst of
            [] -> acc
            y:ys -> case isRev of
                    False -> iter ys y ((oldval <= y) && acc)
                    True -> iter ys y ((oldval >= y) && acc)
    in iter xs x True

appendR, appendI :: [a] -> [a] -> [a]
appendR [] yss = yss
appendR (x:xs) yss = x : appendR xs yss

appendI xss yss = iter (reverse xss) yss
  where iter [] acc = acc
        iter (y:ys) acc = iter ys (y : acc)

interleaveR, interleaveI :: [a] -> [a] -> [a]
interleaveR xss yss = case (xss, yss) of
    (xs, []) -> xs
    ([] , ys) -> ys
    (x:xs, ys) -> x : interleaveR ys xs

interleaveI xss yss = 
    let iter wss zss acc = case (wss, zss) of
            ([], ys) -> reverse acc ++ ys
            (xs, []) -> reverse acc ++ xs
            (x:xs, y:ys) -> iter xs ys (y : x : acc)
    in iter xss yss []

map2R, map2I :: (a -> b -> c) -> [a] -> [b] -> [c]
map2R proc xss yss = case (xss, yss) of
    ([], _) -> []
    (_, []) -> []
    (x:xs, y:ys) -> proc x y : map2R proc xs ys

map2I proc xss yss = 
    let iter wss zss acc = case (wss,zss) of
            ([], _) -> acc
            (_, []) -> acc
            (x:xs, y:ys) -> iter xs ys (proc x y : acc)
    in reverse $ iter xss yss []

zipR, zipI, zipM :: [a] -> [b] -> [(a, b)]
zipR xss yss = map2R (,) xss yss
zipI xss yss = map2I (,) xss yss
zipM xss yss = zipWith (,) xss yss

unzip2I :: [(a, b)] -> ([a], [b])
unzip2I xs = iter (reverse xs) ([], [])
  where iter [] acc = acc
        iter (y:ys) acc = iter ys (fst y : fst acc, snd y : snd acc)

concatR, concatI :: [[a]] -> [a]
concatR [] = []
concatR (x:xs) = x ++ concatR xs

concatI [] = []
concatI (x:xs) = iter (reverse x) xs
  where iter acc [] = reverse acc
        iter acc (y:ys) = iter (reverse y ++ acc) ys


reverseILog :: [a] -> Writer.Writer [(Log.Priority, String)] [a]
reverseILog xs = do
    Writer.tell [(Log.INFO, "reverseI()")]
    return $ reverseI xs


libmain :: IO ()
libmain = do
    putStrLn $ "reverseI " ++ (show [0,1,2,3]) ++ ": " ++ (show $ reverseI [0,1,2,3])
