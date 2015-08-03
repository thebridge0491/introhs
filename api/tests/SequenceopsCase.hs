module SequenceopsCase (tcases) where

import qualified System.IO as IO
import qualified Text.Printf as Printf
import qualified Control.Exception as Exception
import Data.List

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

caseFindIndex = withFixture1 "caseFindIndex" $ mapM_ (\(funcA, nm) -> do
        assertEqual nm (findIndex boolOp lst) (funcA 0 boolOp lst)
        assertEqual nm (findIndex boolOp revlst) (funcA 0 boolOp revlst)
        ) [(findIndexR, "findIndexR"), (findIndexI, "findIndexI")]
    where boolOp = (== 3)

caseReverse = mapM_ (\((funcA, nm), lst) -> 
        assertEqual nm (reverse lst) (funcA lst)
        ) [(f, l) | f <- [(reverseR, "reverseR"), (reverseI, "reverseI")], 
            l <- [lst, revlst]]


--tcases = map (\(nm, f) -> uncurry testCase $ (nm, withFixture1 nm f))
tcases = map (uncurry testCase)
    [("caseFindIndex", caseFindIndex), ("caseReverse", caseReverse)]
