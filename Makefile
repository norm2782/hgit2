CHSS := $(patsubst src/Data/HGit2/%.chs, src/Data/HGit2/%.hs,$(wildcard src/Data/HGit2/*.chs))

default: Main

Main: src/Main.hs $(CHSS)
	cd src && ghc --make Main.hs -lgit2

%.hs: %.chs
	c2hs --cppopts='-I.' --cppopts='-U __BLOCKS__' $<

clean:
	rm -rf dist src/Data/HGit2/*.hs
	find . -iname "*.chi" -o -iname "*.chs.h" -o -iname "*.[o|i]" -o -iname "*.hi"

.PHONY: clean
