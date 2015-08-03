#!/bin/sh
{-
EXTRA_PKGDB=`find $HOME/.stack/snapshots -type f -name package.cache -exec dirname {} \;`
DEPNS_PKGIDS=`for pkg in base test-framework-hunit test-framework-quickcheck2 hslogger ConfigFile regex-posix ; do echo -n -package-id= ; ghc-pkg --global --user --package-db=$EXTRA_PKGDB --simple-output field $pkg id | head -n1 ; done`

#exec ghci -v1 -Wall -fno-warn-type-defaults -fno-warn-missing-signatures -rtsopts -with-rtsopts=-N -i -isrc:tests -global-package-db -user-package-db -package-db=$EXTRA_PKGDB $DEPNS_PKGIDS $0
exec runhaskell -v1 -Wall -fno-warn-type-defaults -fno-warn-missing-signatures -rtsopts -with-rtsopts=-N -i -isrc:tests -global-package-db -user-package-db -package-db=$EXTRA_PKGDB $DEPNS_PKGIDS $0 $@
-}
-- #!/usr/bin/env runhaskell

module Main (main) where

import Test.Framework

import qualified NewCase
import qualified NewProp

{-
mainWithOpts :: IO ()
mainWithOpts = do
    let blank_topts = mempty :: TestOptions
    let topts = blank_topts {
        topt_maximum_generated_tests = Just 30
    }
    let blank_runnerOpts = mempty :: RunnerOptions
    let runnerOpts = blank_runnerOpts {
        ropt_test_options = Just topts
    }
    defaultMainWithOpts tests runnerOpts
-}
main :: IO ()
main = defaultMain tests

tests = map (uncurry testGroup) 
    [("NewCase", NewCase.tcases), ("NewProp", NewProp.tprops)
    ]

{-
:main arg1 argN
-}
