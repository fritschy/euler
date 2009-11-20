all: Main.hs
	ghc -O3 --make Main.hs

main: Main.hs

Main.hs: MakeMain.hs $(wildcard Euler*hs)
	runhaskell MakeMain.hs

clean:
	rm -f *.o *.hi Main Main.hs
