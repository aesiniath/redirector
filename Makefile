all: experiment

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=>/dev/null
endif

experiment: Experiment.hs
	ghc --make -O -o experiment Experiment.hs

clean:
	-rm -f *.hi *.o experiment
