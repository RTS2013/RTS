$name=$args[0]
cls
rm src/build/$name*
ghc -threaded -rtsopts -O2 -Wall `
	$name `
    -odir src/build `
    -hidir src/build `
    -isrc