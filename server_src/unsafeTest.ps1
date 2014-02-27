$name=$args[0]

ghc --make -threaded -rtsopts -O2 -Wall `
    -o build/RTS `
    -odir build `
    -hidir build `
    -main-is $name $name `
    -iSayHi
build/RTS.exe +RTS -N4 -A4M -H1G
rm -Recurse build/*