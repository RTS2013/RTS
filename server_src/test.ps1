$name=$args[0]

ghc -threaded -O2 -Wall -o build/RTS -odir build -hidir build --make -main-is $name $name
cd build
./RTS.exe
rm *
cd ..