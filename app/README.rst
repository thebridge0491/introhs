Introhs.Intro
===========================================
.. .rst to .html: rst2html5 foo.rst > foo.html
..                pandoc -s -f rst -t html5 -o foo.html foo.rst

Main app sub-package for Haskell Intro examples project.

Installation
------------
source code tarball download:
    
        # [aria2c --check-certificate=false | wget --no-check-certificate | curl -kOL]
        
        FETCHCMD='aria2c --check-certificate=false'
        
        $FETCHCMD https://bitbucket.org/thebridge0491/introhs/[get | archive]/master.zip

version control repository clone:
        
        git clone https://bitbucket.org/thebridge0491/introhs.git

build example with make:
cd <path> ; cabal configure --package-db=dist/package.conf.inplace

make build [test]

cabal install introhs-intro

build example with cabal:
cd <path> ; cabal configure ; cabal sandbox init

cabal build introhs-intro [; cabal test introhs-intro]

cabal --require-sandbox install introhs-intro

build example with stack:
cd <path>

stack build introhs-intro [; stack test introhs-intro]

stack install introhs-intro

Usage
-----
        [env RSRC_PATH=<path>/resources] [[stack | cabal] exec | dist/build/]Main-introhs-intro [-h]

or

        [env RSRC_PATH=<path>/resources] sh src/Data/Introhs/Main.hs

or

        ghci -i -isrc src/Data/Introhs/Main.hs

        > :main

Author/Copyright
----------------
Copyright (c) 2015 by thebridge0491 <thebridge0491-codelab@yahoo.com>

License
-------
Licensed under the Apache-2.0 License. See LICENSE for details.
