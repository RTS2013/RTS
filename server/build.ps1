$name=$args[0]
cls
ghc -threaded -rtsopts -O2 -Wall `
	$name `
    -o src/build/$name `
    -odir src/build `
    -hidir src/build `
    -main-is $name `
    -isrc