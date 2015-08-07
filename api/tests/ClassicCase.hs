module ClassicCase (tcases) where

import qualified System.IO as IO
import qualified Text.Printf as Printf
import qualified Control.Exception as Exception
import qualified Control.Monad.Writer as Writer

import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Introhs.Util as Util
import Data.Introhs.Practice.Classic

withFixture1 :: String -> IO a -> IO ()
withFixture1 nm f = Exception.bracket setUp (\_ -> f) tearDown
  where
    setUp = Printf.hPrintf IO.stderr "(%s) SetUp ...\n" nm
    tearDown _ = Printf.hPrintf IO.stderr "... TearDown (%s)\n" nm

epsilon = 0.001
(lst, revlst) = ([0..4], reverse (enumFromTo 0 4))

caseSquare = mapM_ (\n -> do
    mapM_ (\(funcA, nm) -> assertEqual nm (n ** 2.0) (funcA n)
        ) [(squareR, "squareR"), (squareI, "squareI")]
    ) [2, 11, 20]

caseExpt = mapM_ (\(b, n) -> do
    let ans = b ** n
    mapM_ (\(funcA, nm) -> 
        --assertEqual nm (b ** n) (funcA b n)
        assertBool nm (Util.inEpsilon (epsilon * ans) ans (funcA b n))
        ) [(exptR, "exptR"), (exptI, "exptI")
            , (fastExptR, "fastExptR"), (fastExptI, "fastExptI")]
    ) [(b, n) | b <- [2.0, 11.0, 20.0], n <- [3.0, 6.0, 10.0]]

caseSumTo = mapM_ (\(hi, lo) -> do
    let ans = foldl (+) lo [(lo + 1)..hi]
    mapM_ (\(funcA, nm) -> assertEqual nm ans (funcA hi lo)
        ) [(sumToR, "sumToR"), (sumToI, "sumToI")]
    ) [(hi, lo) | hi <- [-15, 0, 150], lo <- [-20, 0, 10]]

caseFact = withFixture1 "caseFact" $ mapM_ (\n -> do
    let ans = foldl (*) 1 [1..(fromIntegral n)]
    mapM_ (\(funcA, nm) -> 
        -- assertEqual nm (product [1..n]) (funcA n)
        assertEqual nm ans (funcA n)
        ) [(factR, "factR"), (factI, "factI")]
    ) [0, 9, 18]

caseFib = mapM_ (\n -> do
    let ans = snd $ foldl (\(s0, s1) _ -> (s0 + s1, s0)) (0, 1) [0..n]
    mapM_ (\(funcA, nm) -> assertEqual nm ans (funcA n)
        ) [(fibR, "fibR"), (fibI, "fibI")]
    ) [0, 7, 13]

casePascaltri = do
    mapM_ (\(funcA, nm) -> assertEqual nm res (funcA rows)
        ) [(pascaltriMult, "pascaltriMult")
            , (pascaltriAdd, "pascaltriAdd")]
  where rows = 5
        res = [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1],  [1, 4, 6, 4, 1], 
            [1, 5, 10, 10, 5, 1]]

caseQuotRem = mapM_ (\(a, b) -> do
        assertEqual "quot" (quot a b) (quotM a b)
        assertEqual "rem" (rem a b) (remM a b)
        ) [(a, b) | a <- [10, -10], b <- [3, -3]]

caseDivMod = mapM_ (\(a, b) -> do
        assertEqual "div" (div a b) (divM a b)
        assertEqual "mod" (mod a b) (modM a b)
        ) [(a, b) | a <- [10, -10], b <- [3, -3]]

caseGcdLcm = mapM_ (\((funcGcd, gcdNm), (funcLcm, lcmNm)) -> do
        assertEqual gcdNm 8 (funcGcd [24, 16])
        assertEqual gcdNm 4 (funcGcd [24, 16, 12])
        assertEqual lcmNm 48 (funcLcm [24, 16])
        assertEqual lcmNm 96 (funcLcm [24, 16, 32])
        ) [((gcdR, "gcdR"), (lcmR, "lcmR"))
            , ((gcdI, "gcdI"), (lcmI, "lcmI"))]

caseBaseExpand = mapM_ (\(funcA, nm) -> do
        assertEqual nm [1, 0, 1, 1] (funcA 2 11)
        assertEqual nm [1, 1, 0, 1] (funcA 4 81)
        ) [(baseExpandR, "baseExpandR"), (baseExpandI, "baseExpandI")]

caseBaseTo10 = mapM_ (\(funcA, nm) -> do
        assertEqual nm 11 (funcA 2 [1, 0, 1, 1])
        assertEqual nm 81 (funcA 4 [1, 1, 0, 1])
        ) [(baseTo10R, "baseTo10R"), (baseTo10I, "baseTo10I")]

caseRange = mapM_ (\((func, rgNm), (funcStep, rgStepNm)) -> do
        assertEqual rgNm lst (func 0 4)
        assertEqual rgStepNm revlst (funcStep (-1) 4 0)
        ) [((rangeR, "rangeR"), (rangeStepR, "rangeStepR"))
            , ((rangeI, "rangeI"), (rangeStepI, "rangeStepI"))]

caseCompose = do
        assertBool "compose1" (Util.inEpsilon (epsilon * n) 
            ((** n) . sqrt $ n)  (compose1 (** n) sqrt n))
        assertEqual "compose1" (length . rangeFunc $ m) 
            (compose1 length rangeFunc m)
  where (m, n, rangeFunc) = (5, 2.0, \x -> enumFromTo 0 (x - 1))


caseFactLog = mapM_ (\((funcA, nm), n) -> do
        let (numFact, logFactI) = Writer.runWriter $ funcA n
        assertBool "factILog msg" (foldl (\a (_, msg) -> a && 
            not (null msg)) True logFactI)
        assertEqual nm (foldl (*) 1 [1..(toInteger n)]) numFact
        ) [(f, n) | f <- [(factILog, "factILog")], n <- [0, 9, 18]]


--tcases = map (\(nm, f) -> uncurry testCase $ (nm, withFixture1 nm f))
tcases = map (uncurry testCase)
    [("caseSquare", caseSquare), ("caseExpt", caseExpt)
    , ("caseSumTo", caseSumTo), ("caseFact", caseFact)
    , ("caseFib", caseFib), ("casePascaltri", casePascaltri)
    , ("caseQuotRem", caseQuotRem), ("caseDivMod", caseDivMod)
    , ("caseGcdLcm", caseGcdLcm), ("caseBaseExpand", caseBaseExpand)
    , ("caseBaseTo10", caseBaseTo10), ("caseRange", caseRange)
    , ("caseCompose", caseCompose), ("caseFactLog", caseFactLog)
    ]
