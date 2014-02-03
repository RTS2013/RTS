$name=$args[0]

cls
ghc -threaded -O2 -Wall -odir build -hidir build $name
cd build
rm -Recurse *
cd ..