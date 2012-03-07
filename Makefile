all: redirect

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=>/dev/null
endif

SOURCES=$(shell find . -name '*.hs')

redirect: $(SOURCES)
	ghc --make -O -o redirect Redirector.hs

clean:
	-rm -f *.hi *.o redirect tags
