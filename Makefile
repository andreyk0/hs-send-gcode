
build:
	cabal build

clean:
	cabal clean

deps:
	cabal install --only-dependencies
