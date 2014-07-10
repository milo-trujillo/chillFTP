chillFTP
========

This is a quick and incomplete implementation of an FTP client and server, written in Racket and Haskell, respectively.

Limitations
-----------

Both client and server support passive FTP only.

Dependencies
------------

Dependencies are minimal, requiring few libraries outside the Haskell and Racket development environments.

### Server
The server depends on several standard packages:
* Control.Concurrent
* Network.Socket
* System.IO
* System.Directory

It also requires System.Posix.Daemonize from the [hdaemonize](http://hackage.haskell.org/package/hdaemonize-0.4) library and System.Posix.Syslog from the [hsyslog](http://hackage.haskell.org/package/hsyslog-1.4) library.

### Client
The client needs a recent version of racket, the compiler raco, and:
* racket/base
* racket/tcp
