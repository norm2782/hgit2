HSSRC := src/haskell/Data/HGit2
CHSS  := $(patsubst $(HSSRC)/%.chs, $(HSSRC)/%.hs,$(wildcard $(HSSRC)/*.chs))

default: Main

Main: src/haskell/Main.hs $(CHSS)
	cd src/haskell && ghc --make Main.hs -lgit2 -Wall -fwarn-tabs

%.hs: %.chs
	c2hs --cppopts='-I.' --cppopts='-U __BLOCKS__' $<

clean:
	rm -rf dist $(HSSRC)/*.hs $(HSSRC)/*.h
	find . -iname "*.chi" -o -iname "*.chs.h" -o -iname "*.[o|i]" -o -iname "*.hi"

.PHONY: clean
