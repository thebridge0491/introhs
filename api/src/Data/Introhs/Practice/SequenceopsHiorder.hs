{-# LANGUAGE ParallelListComp  #-}

module Data.Introhs.Practice.SequenceopsHiorder where

import Data.List
--import qualified Data.Maybe as Maybe

import qualified Data.Introhs.Util as Util

tabulateF :: (Int -> a) -> Int -> [a]
tabulateF func cnt = reverse $
    foldl (\a i -> func i : a) [] [0..(cnt - 1)]

lengthF :: [a] -> Int
lengthF xs = foldl (\a _ -> a + 1) 0 xs

nthF :: Int -> [a] -> Maybe a
nthF idx xs = case foldl corp (0, Nothing) xs of (_, t) -> t
  where corp (h, t) el = if h == idx then (h + 1, Just el) else (h + 1, t)

indexFindF :: (Eq a) => Int -> (a -> Bool) -> [a] -> (Maybe Int, Maybe a)
indexFindF ndx pred1 xs =
    let corp (ndx1, (idx, it)) el
            | pred1 el && it == Nothing = (ndx1 + 1, (Just ndx1, Just el))
            | otherwise = (ndx1 + 1, (idx, it))
    in snd $ foldl corp (ndx, (Nothing, Nothing)) xs

findIndexF :: (Eq a) => (a -> Bool) -> [a] -> Maybe Int
findF :: (Eq a) => (a -> Bool) -> [a] -> Maybe a

findIndexF pred1 xs = fst $ indexFindF 0 pred1 xs

findF pred1 xs = snd $ indexFindF 0 pred1 xs

minmaxF :: (Ord a) => [a] -> (a, a)
minmaxF [] = error "empty list"
minmaxF (x:xs) = foldl (\(lo, hi) e -> case (e < lo, e > hi) of
            (True, _) -> (e, hi)
            (_, True) -> (lo, e)
            (_, _) -> (lo, hi)
        ) (x, x) xs

minF, maxF :: (Ord a) => [a] -> a
minF xs = (fst . minmaxF) xs
maxF xs = (snd . minmaxF) xs

reverseF, copyF :: [a] -> [a]
reverseF xs = foldl (flip (:)) [] xs
copyF xs = foldr (:) [] xs

splitAtF :: Int -> [a] -> ([a], [a])
splitAtF n xs = foldl (\(t, ys) _ -> case ys of
        [] -> (t, ys)
        z:zs -> (z : t, zs))
    ([], xs) [0..(n - 1)]

takeF, dropF :: Int -> [a] -> [a]
takeF n xs = reverse $ fst $ splitAtF n xs
dropF n xs = snd $ splitAtF n xs

anyallF :: (a -> Bool) -> [a] -> (Bool, Bool)
anyallF pred1 xs =
    foldl (\(a0, a1) e -> (a0 || pred1 e, a1 && pred1 e)) (False, True) xs

anyF, allF :: (a -> Bool) -> [a] -> Bool
anyF pred1 xs = fst $ anyallF pred1 xs

allF pred1 xs = snd $ anyallF pred1 xs

mapF :: (a -> b) -> [a] -> [b]
mapF proc xs = foldr (\el acc -> proc el : acc) [] xs

mapM_F :: Monad m => (a -> m b) -> [a] -> m ()
mapM_F proc xs = foldl (\acc el -> proc el >> acc) (return ()) xs

partitionF :: (a -> Bool) -> [a] -> ([a], [a])
partitionF pred1 xs =
    foldl (\(f, r) e -> case pred1 e of
            True -> (e : f, r)
            False -> (f, e : r)
        ) ([], []) (reverse xs)

filterF, removeF :: (a -> Bool) -> [a] -> [a]
filterF pred1 xs = fst $ partitionF pred1 xs
removeF pred1 xs = snd $ partitionF pred1 xs

isOrderedF :: (Ord a) => [a] -> Bool -> Bool
isOrderedF [] _ = True
isOrderedF (x:xs) isRev =
    snd $ foldl (\(oldval, acc) e -> (e, acc && op oldval e)) (x, True) xs
  where op | isRev = (>=) | otherwise = (<=)

appendF, interleaveF :: [a] -> [a] -> [a]
appendF xss yss = foldr (:) yss xss

interleaveF xss yss = fst $ foldr (\e (a, z:zs) -> (z:e:a, zs)) 
        (drop len_short xss ++ drop len_short yss, reverse (take len_short xss))
        (take len_short yss)
  where len_short | length xss < length yss = length xss
            | otherwise = length yss

map2F :: (a -> b -> c) -> [a] -> [b] -> [c]
map2F proc xss yss = reverse $ fst $
    foldl (\(a, (x:xs, y:ys)) _ -> (proc x y : a, (xs, ys))) ([], (xss, yss)) 
        [0..(len_short - 1)]
  where len_short | length xss < length yss = length xss
            | otherwise = length yss

zipF :: [a] -> [b] -> [(a, b)]
zipF xss yss = map2F (,) xss yss

unzip2F :: [(a, b)] -> ([a], [b])
unzip2F xs = foldr proc ([], []) xs
  where proc (eh, et) (ah, at) = (eh:ah, et:at)

concatF :: [[a]] -> [a]
concatF xss = foldr (\e a -> e ++ a) [] xss


tabulateU :: (Int -> a) -> Int -> [a]
tabulateU func cnt = reverse $ Util.headOr [] $ reverse $ unfoldr fn (0, [])
  where fn (idx, acc) | cnt <= idx = Nothing
            | otherwise = Just (func idx : acc, (idx + 1, func idx : acc))

lengthU :: [a] -> Int
lengthU xs = Util.headOr 0 $ reverse $ unfoldr func (0, xs)
  where func (_, []) = Nothing
        func (h, _:ys) = Just (h + 1, (h + 1, ys))

nthU :: Int -> [a] -> Maybe a
nthU idx xs = Util.headOr Nothing $ reverse $ unfoldr func (0, Nothing, xs)
  where genFunc z el ndx | idx == ndx = Just z | otherwise = el
        func (_, _, []) = Nothing
        func (i, e, y:ys) = Just (genFunc y e i, (i + 1, genFunc y e i, ys))

indexFindU :: (Eq a) => Int -> (a -> Bool) -> [a] -> (Maybe Int, Maybe a)
indexFindU ndx pred1 xs = Util.headOr (Nothing, Nothing) $ reverse $ 
    unfoldr func (0, (Nothing, Nothing), xs)
  where genFunc z (idx, el) n | pred1 z && el == Nothing = (Just n, Just z)
            | otherwise = (idx, el)
        func (_, _, []) = Nothing
        func (n, a, y:ys) = Just (genFunc y a n, (n + 1, genFunc y a n, ys))

findIndexU :: (Eq a) => (a -> Bool) -> [a] -> Maybe Int
findU :: (Eq a) => (a -> Bool) -> [a] -> Maybe a

findIndexU pred1 xs = fst $ indexFindU 0 pred1 xs

findU pred1 xs = snd $ indexFindU 0 pred1 xs

minmaxU :: (Ord a) => [a] -> (a, a)
minmaxU [] = error "empty list"
minmaxU (x:xs) = Util.headOr (x, x) $ reverse $ unfoldr func ((x, x), xs)
  where genFunc z (lo, hi) | z < lo = (z, hi) | z > hi = (lo, z)
            | otherwise = (lo, hi)
        func ((_, _), []) = Nothing
        func (a, y:ys) = Just (genFunc y a, (genFunc y a, ys))

minU, maxU :: (Ord a) => [a] -> a
minU xs = (fst . minmaxU) xs
maxU xs = (snd . minmaxU) xs

reverseU, copyU :: [a] -> [a]
reverseU xs = reverse $ unfoldr func xs
  where func [] = Nothing
        func (t:ts) = Just (t, ts)

copyU xs = unfoldr func xs
  where func [] = Nothing
        func (y:ys) = Just (y, ys)

splitAtU :: Int -> [a] -> ([a], [a])
splitAtU n xs = Util.headOr ([], xs) $ reverse $ unfoldr func (n, ([], xs))
  where func (ct, (t, d)) | 0 == ct = Nothing
            | otherwise = case d of
                [] -> Nothing
                y:ys -> Just ((y:t, ys), (ct - 1, (y:t, ys)))

takeU, dropU :: Int -> [a] -> [a]
takeU n xs = reverse $ fst $ splitAtU n xs
dropU n xs = snd $ splitAtU n xs

anyallU :: (a -> Bool) -> [a] -> (Bool, Bool)
anyallU pred1 xs = Util.headOr (False, True) $ reverse $
    unfoldr func ((False, True), xs)
  where func (_, []) = Nothing
        func ((a0, a1), y:ys) = Just ((a0 || pred1 y, a1 && pred1 y), 
            ((a0 || pred1 y, a1 && pred1 y), ys))

anyU, allU :: (a -> Bool) -> [a] -> Bool
anyU pred1 xs = fst $ anyallU pred1 xs

allU pred1 xs = snd $ anyallU pred1 xs

mapU :: (a -> b) -> [a] -> [b]
mapU proc xs = unfoldr func xs
  where func [] = Nothing
        func (y:ys) = Just (proc y, ys)

mapM_U :: Monad m => (a -> m b) -> [a] -> m ()
mapM_U proc xs = Util.headOr (return ()) $ unfoldr func (reverse xs)
  where func [] = Nothing
        func (y:ys) = Just (proc y >> return (), ys)

partitionU :: (a -> Bool) -> [a] -> ([a], [a])
partitionU pred1 xs = Util.headOr ([], xs) $ reverse $
    unfoldr func (([], []), reverse xs)
  where func ((_, _), []) = Nothing
        func ((f, r), y:ys) = case pred1 y of
                True -> Just ((y : f, r), ((y : f, r), ys))
                False -> Just ((f, y: r), ((f, y: r), ys))

filterU, removeU :: (a -> Bool) -> [a] -> [a]
filterU pred1 xs = fst $ partitionU pred1 xs
removeU pred1 xs = snd $ partitionU pred1 xs

isOrderedU :: (Ord a) => [a] -> Bool -> Bool
isOrderedU [] _ = True
isOrderedU (x:xs) isRev = Util.headOr True $ reverse $ unfoldr func (True, (x, xs))
  where op | isRev = (>=) | otherwise = (<=)
        func (_, (_, [])) = Nothing
        func (acc, (y, z:zs)) = Just (acc && op y z, (acc && op y z, (z, zs)))

appendU, interleaveU :: [a] -> [a] -> [a]
appendU xss yss = Util.headOr yss $ reverse $ 
    unfoldr func (yss, reverse xss)
  where func (_, []) = Nothing
        func (h, x:xs) = Just (x : h, (x : h, xs))

interleaveU xss yss = unfoldr func (xss, yss)
  where func (wss, zss) = case (wss, zss) of
            ([], []) -> Nothing
            ([], z:zs) -> Just (z, (zs, []))
            (w:ws, zs) -> Just (w, (zs, ws))

map2U :: (a -> b -> c) -> [a] -> [b] -> [c]
map2U proc xss yss = unfoldr func (xss, yss)
  where func (wss, zss) = case (wss, zss) of
            ([], _) -> Nothing
            (_, []) -> Nothing
            (w:ws, z:zs) -> Just (proc w z, (ws, zs))

zipU :: [a] -> [b] -> [(a, b)]
zipU xss yss = map2U (,) xss yss

unzip2U :: [(a, b)] -> ([a], [b])
unzip2U xss = Util.headOr ([], []) $ reverse $
    unfoldr func (([], []), reverse xss)
  where func (_, []) = Nothing
        func ((ah, at), y:ys) = Just ((fst y : ah, snd y : at), 
                ((fst y : ah, snd y : at), ys))

concatU :: [[a]] -> [a]
concatU xss = Util.headOr [] $ reverse $
    unfoldr func ([], reverse xss)
  where func (_, []) = Nothing
        func (acc, y:ys) = Just (y ++ acc, (y ++ acc, ys))


tabulateLc :: (Int -> a) -> Int -> [a]
tabulateLc func cnt = [func i | i <- [0..(cnt-1)]]

nthLc :: Int -> [a] -> Maybe a
nthLc idx xs = Util.headOr Nothing _helperLc
  where _helperLc = [Just e | (i, e) <- zip [0..(length xs)] xs, idx == i]

indexFindLc :: (Eq a) => Int -> (a -> Bool) -> [a] -> (Maybe Int, Maybe a)
indexFindLc ndx pred1 xs = Util.headOr (Nothing, Nothing) _helperLc
  where _helperLc = [(Just i, Just e) | (i, e) <- 
            zip [0..(length xs)] (drop ndx xs), pred1 e]

findIndexLc :: (Eq a) => (a -> Bool) -> [a] -> Maybe Int
findLc :: (Eq a) => (a -> Bool) -> [a] -> Maybe a

--findIndexLc pred1 xs = Util.headOr Nothing _helperLc
--  where _helperLc = [Just i | (i, e) <- zip [0..(length xs)] xs, pred1 e]
findIndexLc pred1 xs = fst $ indexFindLc 0 pred1 xs

--findLc pred1 xs = Util.headOr Nothing _helperLc
--  where _helperLc = [Just e | e <- xs, pred1 e]
findLc pred1 xs = snd $ indexFindLc 0 pred1 xs

copyLc :: [a] -> [a]
copyLc xs = [e | e <- xs]

takeLc, dropLc :: Int -> [a] -> [a]
takeLc n xs = [e | (i, e) <- zip [0..(length xs)] xs, n > i]
dropLc n xs = [e | (i, e) <- zip [0..(length xs)] xs, not $ n > i]

mapLc :: (a -> b) -> [a] -> [b]
mapLc proc xs = [proc e | e <- xs]

mapM_Lc :: Monad m => (a -> m b) -> [a] -> m ()
mapM_Lc proc xs = Util.headOr (return ()) _helperLc
  where _helperLc = [proc e >> return () | e <- xs]

filterLc, removeLc :: (a -> Bool) -> [a] -> [a]
filterLc pred1 xs = [e | e <- xs, pred1 e]
removeLc pred1 xs = [e | e <- xs, not $ pred1 e]

isOrderedLc :: (Ord a) => [a] -> Bool -> Bool
isOrderedLc [] _ = True
isOrderedLc (x:xs) isRev = _helperLc
  where op | isRev = (>=) | otherwise = (<=)
        _helperLc = all (\e -> True == e) [op a b | (a, b) <- zip (x:xs) xs]

interleaveLc :: [a] -> [a] -> [a]
interleaveLc xss yss =
    (concat [[a, b] | (a, b) <- zip xss yss]) ++
        (drop (length yss) xss ++ drop (length xss) yss)

map2Lc :: (a -> b -> c) -> [a] -> [b] -> [c]
map2Lc proc xss yss = [proc x y | x <- xss | y <- yss]

zipLc :: [a] -> [b] -> [(a, b)]
zipLc xss yss = map2Lc (,) xss yss
