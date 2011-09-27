all: Main.hs
	ghc -O -rtsopts=all -optc-march=native -optc-O3 -threaded --make Main.hs

prof: Main.hs
	ghc -O -rtsopts=all -prof -auto-all -optc-march=native -optc-O3 -threaded --make Main.hs

noopt: Main.hs
	ghc -rtsopts=all -optc-march=native -optc-O3 -threaded --make Main.hs

main: Main.hs

Main.hs: MakeMain.hs $(wildcard Euler*hs)
	runhaskell MakeMain.hs

clean:
	rm -f *.o *.hi Main Main.hs
