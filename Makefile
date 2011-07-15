default: git2.o

git2.o: git2.hs
	ghc --make git2.hs

git2.hs: git2.chs
	c2hs --cppopts='-I.' --cppopts='-U __BLOCKS__' git2.chs

clean:
	rm *.chi *.chs.h *.hs *.o *.hi

.PHONY: clean
