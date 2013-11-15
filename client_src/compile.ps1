ghc Main.hs -o build/RTS -threaded -odir build -hidir build -O2
cd build
./RTS.exe
cd ..