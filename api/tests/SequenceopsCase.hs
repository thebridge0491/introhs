module SequenceopsCase (tcases) where

import qualified System.IO as IO
import qualified Text.Printf as Printf
import qualified Control.Exception as Exception
import Data.List
import qualified Control.Monad.Writer as Writer

import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Introhs.Util as Util
import Data.Introhs.Practice.Sequenceops

withFixture1 :: String -> IO a -> IO ()
withFixture1 nm f = Exception.bracket setUp (\_ -> f) tearDown
  where
    setUp = Printf.hPrintf IO.stderr "(%s) SetUp ...\n" nm
    tearDown _ = Printf.hPrintf IO.stderr "... TearDown (%s)\n" nm

--epsilon = 0.001
(lst, revlst) = ([0..4], reverse (enumFromTo 0 4))

caseTabulate = mapM_ (\((funcA, nm), n) -> do
        assertEqual nm (reverse $ foldl (\a e -> e : a) [] [0..(n - 1)])
            (funcA (\e -> e) n)
        assertEqual nm (reverse $ foldl (\a e -> e : a) [] [2..(n - 1 + 2)])
            (funcA (\e -> e + 2) n)
        ) [(f, n) | f <- [(tabulateR, "tabulateR")
            , (tabulateI, "tabulateI")], n <- [3, 5, 7]]

caseLength = mapM_ (\((funcA, nm), n) -> do
        assertEqual nm (length [0..(n - 1)]) (funcA [0..(n - 1)])
        assertEqual nm (length (enumFromTo 0 (n - 1))) 
            (funcA (enumFromTo 0 (n - 1)))
        ) [(f, n) | f <- [(lengthR, "lengthR"), (lengthI, "lengthI")], 
            n <- [3, 5, 7]]

caseNth = mapM_ (\((funcA, nm), xs) ->
        assertEqual nm (Just $ xs !! 3) (funcA 3 xs)
        ) [(f, l) | f <- [(nthR, "nthR"), (nthI, "nthI")],
            l <- [lst, revlst]]

caseFindIndex = withFixture1 "caseFindIndex" $ mapM_ (\(funcA, nm) -> do
        assertEqual nm (findIndex boolOp lst) (funcA boolOp lst)
        assertEqual nm (findIndex boolOp revlst) (funcA boolOp revlst)
        ) [(findIndexR, "findIndexR"), (findIndexI, "findIndexI")]
    where boolOp = (== 3)

caseFind = mapM_ (\((funcA, nm), xs) ->
        assertEqual nm (find boolOp lst) (funcA boolOp xs)
        ) [(f, l) | f <- [(findR, "findR"), (findI, "findI")],
            l <- [lst, revlst]]
  where boolOp = (== 3)

caseMinMax = mapM_ (\(((funcMin, nmMin), (funcMax, nmMax)), xs) -> do
        assertEqual nmMin (minimum xs) (funcMin xs)
        assertEqual nmMax (maximum xs) (funcMax xs)
        ) [(f, l) | f <- [((minR, "minR"), (maxR, "maxR"))
            , ((minI, "minI"), (maxI, "maxI"))],
            l <- [lst :: [Int], revlst :: [Int]]]

caseReverse = mapM_ (\((funcA, nm), xs) -> 
        assertEqual nm (reverse xs) (funcA xs)
        ) [(f, l) | f <- [(reverseR, "reverseR"), (reverseI, "reverseI")], 
            l <- [lst, revlst]]

caseCopy = mapM_ (\((funcA, nm), xs) ->
        assertEqual nm xs (funcA xs)
        ) [(f, l) | f <- [(copyR, "copyR"), (copyI, "copyI")],
            l <- [lst, revlst]]

caseTakeDrop =  mapM_ (\(((funcTake, nmTake), (funcDrop, nmDrop)), xs) -> do
        assertEqual nmTake (take n xs) (funcTake n xs)
        assertEqual nmDrop (drop n xs) (funcDrop n xs)
        ) [(f, l) | f <- [((takeI, "takeI"), (dropI, "dropI"))],
            l <- [lst, revlst]]
  where n = 3

caseAnyAll = do
        mapM_ (\((funcAny, nmAny), (funcAll, nmAll)) -> do
            assertEqual nmAny (any boolOp1 lst1) (funcAny boolOp1 lst1)
            assertEqual nmAll (all boolOp1 lst3) (funcAll boolOp1 lst3)
            ) [((anyR, "anyR"), (allR, "allR")), 
                ((anyI, "anyI"), (allI, "allI"))]
        mapM_ (\((funcAny, nmAny), (funcAll, nmAll)) -> do
            assertEqual nmAny (any boolOp2 lst2) (funcAny boolOp2 lst2)
            assertEqual nmAll (all boolOp2 lst4) (funcAll boolOp2 lst4)
            ) [((anyR, "anyR"), (allR, "allR")), 
                ((anyI, "anyI"), (allI, "allI"))]
  where (boolOp1, boolOp2) = (even, \el -> [] /= el)
        (lst1, lst2) = ([1, 2, 3], [[1, 2], [], [3, 4]])
        (lst3, lst4) = ([6, 2, 4], [[1, 2], [5], [3, 4]])

caseMap = mapM_ (\((funcA, nm), xs) -> 
        assertEqual nm (map proc xs) (funcA proc xs)
        ) [(f, l) | f <- [(mapR, "mapR"), (mapI, "mapI")],
            l <- [lst, revlst]]
  where proc = (+ 2)

caseMapM_ = mapM_ (\((funcA, _), xs) -> 
        --assertEqual nm (mapM_ proc xs) (funcA proc xs))
        --(mapM_ proc xs)
        funcA proc xs
        ) [(f, l) | f <- [(mapM_R, "mapM_R"), (mapM_I, "mapM_I")],
            l <- [lst, revlst]]
  where proc = Printf.printf "%d "

caseFilterRemove = mapM_ (\(((funcFilter, nmFilter), (funcRemove, nmRemove)), xs) -> do
        assertEqual nmFilter (filter even xs) (funcFilter even xs)
        assertEqual nmRemove (snd $ partition even xs) (funcRemove even xs)
        ) [(f, l) | f <- [((filterR, "filterR"), (removeR, "removeR")),
            ((filterI, "filterI"), (removeI, "removeI"))],
            l <- [lst, revlst]]

caseFoldl = mapM_ (\((funcA, nm), xs) -> do
        assertEqual nm (foldl corp1 0 xs) (funcA corp1 0 xs)
        assertEqual nm (foldl corp2 0 xs) (funcA corp2 0 xs)
        ) [(f, l) | f <- [(foldlR, "foldlR"), (foldlI, "foldlI")],
            l <- [lst, revlst]]
  where (corp1, corp2) = ((+), (-))

caseFoldr = mapM_ (\((funcA, nm), xs) -> do
        assertEqual nm (foldr proc1 0 xs) (funcA proc1 0 xs)
        assertEqual nm (foldr proc2 0 xs) (funcA proc2 0 xs)
        ) [(f, l) | f <- [(foldrR, "foldrR"), (foldrI, "foldrI")],
            l <- [lst, revlst]]
  where (proc1, proc2) = ((+), (-))

caseUnfoldright = do
        assertEqual "unfoldright" (unfoldr func1 seed1)
            (reverse $ unfoldrightI func1 seed1)
        assertEqual "unfoldright" (unfoldr func2 seed2)
            (reverse $ unfoldrightI func2 seed2)
  where func1 (h, t) = if t == 0 then Nothing else Just (h, (h + 1, t - h))
        func2 (h, t) = if t == 0 then Nothing else Just (h, (h + 1, h - t))
        (seed1, seed2) = ((0, 10), (0, 2))

caseUnfoldleft = do
        assertEqual "unfoldleft" (unfoldr func1 seed1)
            (unfoldleftR func1 seed1)
        assertEqual "unfoldleft" (unfoldr func2 seed2)
            (unfoldleftR func2 seed2)
  where func1 (h, t) = if t == 0 then Nothing else Just (h, (h + 1, t - h))
        func2 (h, t) = if t == 0 then Nothing else Just (h, (h + 1, h + t))
        (seed1, seed2) = ((0, 10), (0, -10))

caseIsOrdered = do
    assertEqual "isOrderedR" (verifyfn (<=) lst) (isOrderedR lst False)
    assertEqual "isOrderedI" (verifyfn (>=) revlst)
        (isOrderedI revlst True)
    assertEqual "isOrderedR" (verifyfn (<=) chars1)
        (isOrderedR chars1 False)
    assertEqual "isOrderedI" (verifyfn (>=) ltrs1) (isOrderedI ltrs1 True)
  where verifyfn _ [] = True
        verifyfn cmpfn (x:xs) = 
            fst $ foldl (\(a, cur) e -> ((cmpfn cur e) && a, e)) (True, x) xs
        chars1 = ['a', 'c', 'e']
        ltrs1 = ["9", "5", "2"]

caseAppend = mapM_ (\((funcA, nm), xs) -> 
        assertEqual nm (xs ++ [9, 9, 9, 9]) (funcA xs [9, 9, 9, 9])
        ) [(f, l) | f <- [(appendR, "appendR"), (appendI, "appendI")],
            l <- [lst, revlst]]

caseInterleave = mapM_ (\(funcA, nm) ->
        assertEqual nm [0, 9, 1, 9, 2, 9, 3, 9, 4] (funcA lst lst2)
        ) [(interleaveR, "interleaveR"), (interleaveI, "interleaveI")]
  where lst2 = [9, 9, 9, 9]

caseMap2 = mapM_ (\((funcA, nm), xs) -> 
        assertEqual nm (zipWith proc xs xs) (funcA proc xs xs)
        ) [(f, l) | f <- [(map2R, "map2R"), (map2I, "map2I")],
            l <- [lst, revlst]]
  where proc e1 e2 = (e1 + e2) + 2

caseZip = mapM_ (\(funcA, nm) -> 
        assertEqual nm (zip lst1 lst2) (funcA lst1 lst2)
        ) [(zipR, "zipR"), (zipI, "zipI"), (zipM, "zipM")]
  where (lst1, lst2) = ([0, 1, 2], [20, 30, 40])

caseUnzip = mapM_ (\(funcA, nm) -> 
        assertEqual nm (unzip xs) (funcA xs)
        ) [(unzip2I, "unzip2I")]
  where xs = [(0, 20), (1, 30)]

caseConcat = do
        assertEqual "concatR" (concat nlst1) (concatR nlst1)
        assertEqual "concatR" (concat nlst2) (concatR nlst2)
        assertEqual "concatI" (concat nlst1) (concatI nlst1)
        assertEqual "concatI" (concat nlst2) (concatI nlst2)
  where nlst1 = [[0, 1, 2], [20, 30]]
        nlst2 = [[[0, 1]], [], [[20, 30]]]


caseReverseLog = mapM_ (\((funcA, nm), xs) -> do
        let (lstRev, logReverseI) = Writer.runWriter $ funcA xs
        assertBool "reverseILog msg" (foldl (\a (_, msg) -> a && 
            not (null msg)) True logReverseI)
        assertEqual nm (reverse xs) lstRev
        ) [(f, l) | f <- [(reverseILog, "reverseILog")], 
            l <- [lst, revlst]]


--tcases = map (\(nm, f) -> uncurry testCase $ (nm, withFixture1 nm f))
tcases = map (uncurry testCase)
    [("caseTabulate", caseTabulate), ("caseLength", caseLength)
    , ("caseNth", caseNth), ("caseFindIndex", caseFindIndex)
    , ("caseFind", caseFind), ("caseMinMax", caseMinMax)
    , ("caseReverse", caseReverse), ("caseCopy", caseCopy)
    , ("caseTakeDrop", caseTakeDrop), ("caseAnyAll", caseAnyAll)
    , ("caseMap", caseMap), ("caseMapM_", caseMapM_)
    , ("caseFilterRemove", caseFilterRemove)
    , ("caseFoldl", caseFoldl), ("caseFoldr", caseFoldr)
    , ("caseUnfoldright", caseUnfoldright)
    , ("caseUnfoldleft", caseUnfoldleft)
    , ("caseIsOrdered", caseIsOrdered)
    , ("caseAppend", caseAppend), ("caseInterleave", caseInterleave)
    , ("caseMap2", caseMap2), ("caseZip", caseZip)
    , ("caseUnzip", caseUnzip), ("caseConcat", caseConcat)
    , ("caseReverseLog", caseReverseLog)
    ]
