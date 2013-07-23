main=anderson
n=30
m=5

GHCOPTS=

files = $(wildcard *.hs)
pictures = $(sort $(patsubst %.gv, %.png, $(wildcard pics/*.gv)))
autopictures = $(shell for i in $$(seq -f %02g 0 $n); do echo pics/graph-$$i.png; done)
# pictures = $(patsubst %.gz,%.png,$(wildcard pics/*.gz))

all: $(main)

$(main): $(files)
	ghc --make $(GHCOPTS) -o $@ $^

pics: $(pictures)

show: pics
	sxiv $(pictures)
	
apics: $(autopictures)

ashow: apics
	sxiv $(autopictures)

$(patsubst %.png,%.gv,$(autopictures)): $(main)
	./$(main) -C --fbase=pics/graph -n$n -m$m

%.png: %.gv
	neato -Tpng -o $@ $<

clean: 
	rm -f *.o *.hi pics/*
