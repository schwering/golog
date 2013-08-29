PROFILING = --enable-library-profiling --enable-executable-profiling
PROFILING =

all: dist
	cabal build
	cabal install --force-reinstalls

dist: golog.cabal
	cabal configure $(PROFILING)

doc:
	cabal haddock --hyperlink-source

clean:
	rm -rf dist

