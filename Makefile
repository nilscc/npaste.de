.PHONY: all build conf doc haddock clean install run

all: conf build

conf:
	cabal configure

build:
	cabal build

clean:
	cabal clean
	-rm npaste.de

doc: haddock

haddock: conf
	cabal haddock --hyperlink-source

install:
	cabal install --dependencies-only
	cabal install --bindir=.

run: install
	./npaste.de
