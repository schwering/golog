all: dist
	cabal build

dist:
	cabal configure

doc:
	cabal haddock --hyperlink-source

clean:
	rm -rf dist

