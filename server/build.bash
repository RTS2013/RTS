#!/bin/bash

clear
ghc -threaded -rtsopts -O2 -Wall \
    $1 \
    -o src/build/$1 \
    -odir src/build \
    -hidir src/build \
    -main-is $1 \
    -isrc
