default: Main.o

Main.o: Main.hs Git2.hs
	ghc --make Main.hs

Git2.hs: Git2.chs
	c2hs --cppopts='-I.' --cppopts='-U __BLOCKS__' --platform='x86_64-darwin' Git2.chs

clean:
	rm *.chi *.chs.h Git2.hs *.o *.hi

.PHONY: clean
