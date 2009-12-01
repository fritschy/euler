all: Main.hs
	ghc -O -optc-march=native -optc-O3 -threaded --make Main.hs

prof: Main.hs
	ghc -O -prof -auto-all -optc-march=native -optc-O3 -threaded --make Main.hs

noopt: Main.hs
	ghc -optc-march=native -optc-O3 -threaded --make Main.hs

main: Main.hs

Main.hs: MakeMain.hs $(wildcard Euler*hs)
	runhaskell MakeMain.hs

clean:
	rm -f *.o *.hi Main Main.hs
