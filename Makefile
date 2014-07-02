# Set compilers
HC = ghc
RC = raco

# Set options for the Haskell and Racket compilers
HFLAGS = -Wall --make
RFLAGS = exe

server:
	$(HC) $(HFLAGS) Server.hs

client:
	$(RC) $(RFLAGS) Client.rkt

all:
	$(HC) $(HFLAGS) Server.hs
	$(RC) $(RFLAGS) Client.rkt

clean:
	rm -f *.o *.hi Client Server
