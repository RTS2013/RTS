cd local_pkg
./configure.ps1
cd ..
ghc Main.hs -o build/RTS -threaded -odir build -hidir build -O2 -Wall