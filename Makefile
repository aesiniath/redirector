all: replace

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=>/dev/null
endif

replace: Replace.hs
	ghc --make -o replace -O Replace.hs

clean:
	-rm -f *.hi *.o replace experiment
