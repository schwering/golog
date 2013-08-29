PROFILING = --enable-library-profiling --enable-executable-profiling
PROFILING =

all: dist
	cabal build

dist: golog-old.cabal
	cabal configure $(PROFILING)

doc:
	cabal haddock --hyperlink-source

clean:
	rm -rf dist

