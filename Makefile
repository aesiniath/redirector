all: redirect

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=>/dev/null
endif

redirect: Redirector.hs
	ghc --make -O -o redirect Redirector.hs

experiment: Experiment.hs
	ghc --make -O -o experiment Experiment.hs

clean:
	-rm -f *.hi *.o experiment redirect
