all: redirect

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=>/dev/null
endif

SOURCES=$(shell find . -name '*.hs')

redirect: $(SOURCES)
	stack build

clean:
	-rm -f *.hi *.o redirect codex.tags
