default: dist/setup-config
	cabal build

dist/setup-config: hgit2.cabal
	cabal configure

clean:
	cabal clean

.PHONY: clean
