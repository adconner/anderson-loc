main=anderson

draw=draw
n=30
m=5

GHCOPTS=

files = $(wildcard *.hs)
pictures = $(sort $(patsubst %.gv, %.png, $(wildcard pics/*.gv)))
autopictures = $(shell for i in $$(seq 0 $n); do echo pics/graph-$$i.png; done)
# pictures = $(patsubst %.gz,%.png,$(wildcard pics/*.gz))

all: $(main)

$(main): $(files)
	ghc --make -main-is Main.Anderson.main $(GHCOPTS) -o $@ $^

$(draw): $(files)
	ghc --make -main-is Main.Draw.main $(GHCOPTS) -o $@ $^

pics: $(pictures)

show: pics
	sxiv $(pictures)
	
apics: $(autopictures)

ashow: apics
	sxiv $(autopictures)

$(patsubst %.png,%.gv,$(autopictures)): $(draw)
	./$(draw) --fbase=pics/graph -n$n -m$m

%.png: %.gv
	neato -Tpng -o $@ $<

clean: 
	rm -f *.o *.hi pics/*
