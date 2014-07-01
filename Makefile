# Set compilers
HC = ghc
RC = racket

server:
	$(HC) Server.hs

client:
	$(RC) Client.rkt

all:
	$(HC) Server.hs
	$(RC) Client.rkt

clean:
	rm -f *.o *.hi Client Server
