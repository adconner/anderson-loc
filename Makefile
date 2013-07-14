GHCOPTS=
files = $(wildcard *.hs)

all: anderson

anderson: $(files)
	ghc --make $(GHCOPTS) -o $@ $^
