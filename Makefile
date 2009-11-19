all: Main.hs
	ghc -O3 --make Main.hs

main: Main.hs

Main.hs: $(wildcard Euler*hs)
	zsh make-main.sh

clean:
	rm -f *.o *.hi Main Main.hs
