callme: callme.hs
	ghc -O2 --make callme

all: callme

clean: callme callme.hi callme.o
	rm callme callme.hi callme.o
