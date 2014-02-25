$name=$args[0]

ghc -threaded -rtsopts -O2 -Wall -o build/RTS -odir build -hidir build --make -main-is $name $name
cd build
./RTS.exe +RTS -N4 -A4M -H1G
rm -Recurse *
cd ..