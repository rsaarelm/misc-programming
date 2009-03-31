all: build/prob.html

build:
	mkdir build

build/prob.html: Prob.lhs header.html build
	pandoc -o $@ -f markdown+lhs Prob.lhs -s -C header.html

clean:
	rm -rf build
	runhaskell Setup.hs clean