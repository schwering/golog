PROFILING = --enable-library-profiling --enable-executable-profiling
PROFILING =

all: dist
	cabal build

dist: torcs-agent.cabal ../golog/dist/build/libHSgolog-3.1.a
	cabal configure $(PROFILING)

doc:
	cabal haddock --hyperlink-source

clean:
	rm -rf dist

