all: dist replay
	cabal build

dist: prgolog.cabal
	cabal configure --enable-library-profiling --enable-executable-profiling

replay: src/replay.c
	cc -Wall -o replay src/replay.c

doc:
	cabal haddock --hyperlink-source

clean:
	rm -rf dist
	rm -f replay

