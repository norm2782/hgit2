HSSRC := src/haskell/Data/HGit2
CHSS  := $(patsubst $(HSSRC)/%.chs, $(HSSRC)/%.hs,$(wildcard $(HSSRC)/*.chs))

default: $(CHSS) dist/setup-config
	cabal build

dist/setup-config: hgit2.cabal
	cabal configure

%.hs: %.chs
	c2hs --cppopts='-I.' --cppopts='-U __BLOCKS__' $<

clean:
	cabal clean
	rm $(HSSRC)/*.chi $(HSSRC)/*.h $(HSSRC)/*.hs

.PHONY: clean
