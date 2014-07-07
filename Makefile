# Set compilers
HC = ghc
RC = raco

# Set options for the Haskell and Racket compilers
HFLAGS = -Wall --make -threaded -with-rtsopts="-N" # -O2
# -threaded enables true multiple system threads
# -with-rtsopts="-N" specifies to make a thread for each core on the host OS
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
