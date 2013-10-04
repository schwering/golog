PROFILING =
PROFILING = --enable-library-profiling

all: dist
	cabal build
	cabal install --force-reinstalls

dist: golog.cabal
	cabal configure --enable-tests $(PROFILING)

doc:
	cabal haddock --hyperlink-source

clean:
	rm -rf dist

