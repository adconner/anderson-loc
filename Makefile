GHCOPTS=

all: anderson

anderson: anderson.hs
	ghc $(GHCOPTS) -o $@ $<
