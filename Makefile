GHCOPTS=
files = $(wildcard *.hs)
pictures = $(patsubst %.gv, %.png, $(wildcard *.gv))

all: anderson

anderson: $(files)
	ghc --make $(GHCOPTS) -o $@ $^

pics: $(pictures)

%.png: %.gv
	neato -Tpng -o $@ $<
