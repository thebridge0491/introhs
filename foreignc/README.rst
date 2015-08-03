Introhs.Foreignc
===========================================
.. .rst to .html: rst2html5 foo.rst > foo.html
..                pandoc -s -f rst -t html5 -o foo.html foo.rst

FFI sub-package for Haskell Intro examples project.

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

cabal install introhs-foreignc

build example with cabal:
cd <path> ; cabal configure ; cabal sandbox init

cabal build introhs-foreignc [; cabal test introhs-foreignc]

cabal --require-sandbox install introhs-foreignc

build example with stack:
cd <path>

stack build introhs-foreignc [; stack test introhs-foreignc]

stack install introhs-foreignc

Usage
-----
        // PKG_CONFIG='pkg-config --with-path=$PREFIX/lib/pkgconfig'
        
        // $PKG_CONFIG --cflags --libs <ffi-lib>

        [LD_LIBRARY_PATH=$PREFIX/lib] ghci -isrc <-lffi-lib> src/Data/Introhs/Classic.hs
        
        > factI 5

Author/Copyright
----------------
Copyright (c) 2015 by thebridge0491 <thebridge0491-codelab@yahoo.com>

License
-------
Licensed under the Apache-2.0 License. See LICENSE for details.
