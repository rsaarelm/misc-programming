CABAL = haskell-prob.cabal
PROGNAME = prob
DOCNAME = prob.html

BINDIR = dist/build/$(PROGNAME)
BINARY = $(BINDIR)/$(PROGNAME)
DOC = dist/$(DOCNAME)

all: $(DOC) $(BINARY)

dist:
	mkdir dist
	cp -r slidy dist/

$(DOC): Prob.lhs header.html dist/
	pandoc -o $@ -f markdown+lhs Prob.lhs -s -C header.html

dist/setup-config: haskell-prob.cabal Setup.hs
	runhaskell Setup configure

$(BINARY): Prob.lhs dist/setup-config
	runhaskell Setup build

run: $(BINARY)
	$(BINARY)

scratch: scratch.hs
	ghc -XGeneralizedNewtypeDeriving -package mtl -o $@ $<

runscratch: scratch
	./scratch

clean:
	runhaskell Setup clean
	rm -f scratch scracth.o scratch.hi