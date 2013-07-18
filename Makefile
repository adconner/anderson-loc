main=anderson

draw=draw
m=4
n=30

GHCOPTS=

files = $(wildcard *.hs)
# pictures = $(patsubst %.gv, %.png, $(wildcard pics/*.gv))
pictures = $(shell for i in $$(seq 0 $n); do echo pics/graph-$$i.png; done)

all: $(main)

$(main): $(files)
	ghc --make -main-is Main.Anderson.main $(GHCOPTS) -o $@ $^

$(draw): $(files)
	ghc --make -main-is Main.Draw.main $(GHCOPTS) -o $@ $^

pics: $(pictures)

show: pics
	sxiv $(pictures)

$(patsubst %.png,%.gv,$(pictures)): $(draw)
	./$(draw) pics/graph $m $n

%.png: %.gv
	neato -Tpng -o $@ $<
