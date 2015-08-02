Introhs.Util
===========================================
.. .rst to .html: rst2html5 foo.rst > foo.html
..                pandoc -s -f rst -t html5 -o foo.html foo.rst

Utilites sub-package for Haskell Intro examples project.

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

cabal install introhs-util

build example with cabal:
cd <path> ; cabal configure ; cabal sandbox init

cabal build introhs-util [; cabal test introhs-util]

cabal --require-sandbox install introhs-util

build example with stack:
cd <path>

stack build introhs-util [; stack test introhs-util]

stack install introhs-util

Usage
-----
        // PKG_CONFIG='pkg-config --with-path=$PREFIX/lib/pkgconfig'
        
        // $PKG_CONFIG --cflags --libs <ffi-lib>

        [LD_LIBRARY_PATH=$PREFIX/lib] ghci -isrc src/Data/Introhs/Util.hs
        
        > cartesianProd [0, 1, 2] [10, 20, 30]

Author/Copyright
----------------
Copyright (c) 2015 by thebridge0491 <thebridge0491-codelab@yahoo.com>

License
-------
Licensed under the Apache-2.0 License. See LICENSE for details.
