$name=$args[0]

ghc -threaded -O2 -Wall -o build/RTS -odir build -hidir build -XSafe --make -main-is $name $name
cd build
./RTS.exe +RTS -N4 -A4M -H4G
rm -Recurse *
cd ..