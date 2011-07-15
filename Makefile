default: git2.hs

git2.hs: git2.chs
	c2hs --cppopts='-I.' --cppopts='-U __BLOCKS__' git2.chs

clean:
	rm *.chi *.chs.h *.hs

.PHONY: clean
