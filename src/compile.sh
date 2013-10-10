cd Local
chmod u+x configure.sh
cd ..
ghc GUI.hs -o build/RTS -threaded -odir build -hidir build -O2
