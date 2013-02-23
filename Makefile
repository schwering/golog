all: dist replay
	cabal build

dist:
	cabal configure

replay: src/replay.c
	cc -Wall -o replay src/replay.c

doc:
	cabal haddock --hyperlink-source

clean:
	rm -rf dist

