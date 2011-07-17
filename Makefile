HSSRC := src/haskell/Data/HGit2
CHSS  := $(patsubst $(HSSRC)/%.chs, $(HSSRC)/%.hs,$(wildcard $(HSSRC)/*.chs))

default: $(CHSS)
	cabal configure
	cabal build

%.hs: %.chs
	c2hs --cppopts='-I.' --cppopts='-U __BLOCKS__' $<

clean:
	cabal clean
	rm $(HSSRC)/*.chi $(HSSRC)/*.h $(HSSRC)/*.hs

.PHONY: clean
