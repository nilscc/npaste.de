.PHONY: all build conf doc haddock clean install

all: conf build

conf:
	cabal configure

build:
	cabal build

clean:
	cabal clean

doc: haddock

haddock:
	cabal haddock --hyperlink-source

install:
	cabal install --bindir=.
