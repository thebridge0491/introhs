module NewCase (tcases) where

import qualified System.IO as IO
import qualified Text.Printf as Printf
import qualified Control.Exception as Exception

import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Introhs.Util as Util

withFixture1 :: String -> IO a -> IO ()
withFixture1 nm f = Exception.bracket setUp (\_ -> f) tearDown
  where
    setUp = Printf.hPrintf IO.stderr "(%s) SetUp ...\n" nm
    tearDown _ = Printf.hPrintf IO.stderr "... TearDown (%s)\n" nm

epsilon = 0.001
--(lst, revlst) = ([0..4], reverse (enumFromTo 0 4))

(case00, case0) = (4 @=? (2 * 2), 4 == (2 * 2) @? "for 4")
case1 = withFixture1 "case00" $ assertEqual "for 4" 4 (2 * 2)
caseStr = assertEqual "for String 'Hello'" "Hello" "Hello"
caseDbl = do
    assertBool "for Double 0.0" $ Util.inEpsilon (epsilon * 0.0) 0.0 0.0
    assertBool "for Double 4.0" $ Util.inEpsilon (epsilon * 4.0) 4.0 4.0001
caseBad = assertBool "for Bool" (4 > 5)
caseFail = assertFailure "for Failure"
caseException = assertEqual "for Exception" 1 (1 / 0)


--tcases = map (\(nm, f) -> uncurry testCase $ (nm, withFixture1 nm f))
tcases = map (uncurry testCase)
    [("case00", case00), ("case0", case0), ("case1", case1)
    , ("caseStr", caseStr), ("caseDbl", caseDbl), ("caseBad", caseBad)
    , ("caseFail", caseFail), ("caseException", caseException)
    ]
