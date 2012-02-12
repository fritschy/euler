GHCOPTS = -rtsopts=all -fllvm -threaded
OPTIMIZE = -O3

all: Main.hs
	ghc $(GHCOPTS) $(OPTIMIZE) --make Main.hs

prof: Main.hs
	ghc $(GHCOPTS) $(OPTIMIZE) -prof -auto-all --make Main.hs

noopt: Main.hs
	ghc $(GHCOPTS) --make Main.hs

main: Main.hs

Main.hs: MakeMain.hs $(wildcard Euler*hs)
	runhaskell MakeMain.hs

clean:
	rm -f *.o *.hi Main Main.hs
