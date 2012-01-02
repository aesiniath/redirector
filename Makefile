all: redirect

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=>/dev/null
endif

redirect: Redirector.hs
	ghc --make -O -o redirect Redirector.hs

clean:
	-rm -f *.hi *.o redirect
