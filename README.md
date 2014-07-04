chillFTP
========

This is a quick and incomplete implementation of an FTP client and server, written in Racket and Haskell, respectively.

Limitations
-----------

Both client and server support passive FTP only.

Dependencies
------------

Dependencies are minimal, and should come with the Haskell and Racket development environments.

### Server
The server depends on:
* Control.Concurrent
* Network.Socket
* System.IO

### Client
The client needs a recent version of racket, the compiler raco, and:
* racket/base
* racket/tcp
