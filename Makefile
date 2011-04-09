.PHONY: all build conf haddock clean install

all: conf build

conf:
	cabal configure

build:
	cabal build

clean:
	cabal clean

haddock:
	cabal haddock --hyperlink-source

install:
	cabal install --bindir=.
