rm CollisionTest.exe
ghc CollisionTest.hs -O2 --make -prof -auto-all -caf-all -fforce-recomp -threaded
.\CollisionTest.exe +RTS -p -hc -N4