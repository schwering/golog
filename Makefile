PROFILING = --enable-library-profiling --enable-executable-profiling
PROFILING =

all: dist replay
	cabal build

dist: prgolog.cabal
	cabal configure $(PROFILING)

replay: scripts/replay.c
	cc -Wall -o replay scripts/replay.c

doc:
	cabal haddock --hyperlink-source

clean:
	rm -rf dist
	rm -f replay

