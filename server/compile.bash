#!/bin/bash
clear
rm src/build/$1*
ghc -threaded -rtsopts -O2 -Wall \
	$1 \
    -odir src/build \
    -hidir src/build \
    -isrc
