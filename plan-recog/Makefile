PROFILING = --enable-library-profiling --enable-executable-profiling
PROFILING =

all: dist replay
	cabal build

dist: plan-recog.cabal ../golog/dist/build/libHSgolog-3.1.a
	cabal configure $(PROFILING)

replay: scripts/replay.c
	cc -Wall -o replay scripts/replay.c

doc:
	cabal haddock --hyperlink-source

clean:
	rm -rf dist
	rm -f replay

