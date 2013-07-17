m=4
n=30

GHCOPTS=

files = $(wildcard *.hs)
# pictures = $(patsubst %.gv, %.png, $(wildcard pics/*.gv))
pictures = $(shell for i in $$(seq 0 $n); do echo pics/graph-$$i.png; done)

all: anderson

anderson: $(files)
	ghc --make $(GHCOPTS) -o $@ $^

pics: $(pictures)

show: pics
	sxiv $(pictures)

$(patsubst %.png,%.gv,$(pictures)): anderson
	./anderson pics/graph $m $n

%.png: %.gv
	neato -Tpng -o $@ $<
