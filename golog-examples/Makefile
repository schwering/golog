PROFILING =
PROFILING = --enable-library-profiling --enable-executable-profiling

all: dist
	cabal build

dist: golog-examples.cabal ../golog/dist/build/libHSgolog-3.1.a
	cabal configure --enable-tests $(PROFILING)

doc:
	cabal haddock --hyperlink-source

clean:
	rm -rf dist

