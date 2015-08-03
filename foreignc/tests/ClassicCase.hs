module ClassicCase (tcases) where

import qualified System.IO as IO
import qualified Text.Printf as Printf
import qualified Control.Exception as Exception

import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Introhs.Util as Util
import Data.Introhs.Classic

withFixture1 :: String -> IO a -> IO ()
withFixture1 nm f = Exception.bracket setUp (\_ -> f) tearDown
  where
    setUp = Printf.hPrintf IO.stderr "(%s) SetUp ...\n" nm
    tearDown _ = Printf.hPrintf IO.stderr "... TearDown (%s)\n" nm

epsilon = 0.001
--(lst, revlst) = ([0..4], reverse (enumFromTo 0 4))

caseFact = withFixture1 "caseFact" $ mapM_ (\n -> do
    let ans = foldl (*) 1 [1..n]
    mapM_ (\(funcA, nm) -> 
        -- assertEqual nm (product [1..n]) (funcA n)
        assertEqual nm ans (funcA n)
        ) [(factLp, "factLp"), (factI, "factI")]
    ) [0, 9, 18]

caseExpt = mapM_ (\(b, n) -> do
    let ans = b ** n
    mapM_ (\(funcA, nm) -> 
        --assertEqual nm (b ** n) (funcA b n)
        assertBool nm (Util.inEpsilon (epsilon * ans) ans (funcA b n))
        ) [(exptLp, "exptLp"), (exptI, "exptI")]
    ) [(x, y) | x <- [2.0, 11.0, 20.0], y <- [3.0, 6.0, 10.0], True]


--tcases = map (\(nm, f) -> uncurry testCase $ (nm, withFixture1 nm f))
tcases = map (uncurry testCase)
    [("caseFact", caseFact), ("caseExpt", caseExpt)]
