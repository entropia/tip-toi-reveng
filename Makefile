tttool: tttool.hs
	ghc -O -with-rtsopts=-K100M  tttool.hs

all: tttool libtiptoi gameanalyse makegraphic

libtiptoi: libtiptoi.c
	gcc -g -o libtiptoi libtiptoi.c

gameanalyse: gameanalyse.c  
	gcc -g -o gameanalyse gameanalyse.c

makegraphic: makegraphic.c
	gcc -g -o makegraphic makegraphic.c -lpng
