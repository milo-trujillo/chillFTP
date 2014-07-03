{-
	This module handles passive data connections for FTP.
	We only export the main function, openPASV, all other code is private.
-}

module PASV (openPASV) where

import System.IO
import Network.Socket
import Control.Concurrent

-- We open one port per user data connection, so the connection limit is 1
max_connections :: Int
max_connections = 1

-- Opens a new passive ftp connection, returns the PASV string
-- PASV response strings are in the format "h1,h2,h3,h4,p1,p2" as described
-- here: http://cr.yp.to/ftp/retr.html
openPASV :: IO Int
openPASV = do
	sock <- socket AF_INET Stream 0		-- Make new socket
	setSocketOption sock ReuseAddr 1	-- Make reusable listening sock
	bindSocket sock (SockAddrInet aNY_PORT iNADDR_ANY)
	listen sock max_connections
	_ <- forkIO (handlePASV sock)
	addr <- getSocketName sock
	putStrLn (show addr)
	portno <- socketPort sock
	return (read (show (portno)) :: Int)

-- Handles a passive ftp connection once a user connects
handlePASV :: Socket -> IO ()
handlePASV pasv = do
	(sock, _) <- Network.Socket.accept pasv
	s <- socketToHandle sock ReadWriteMode
	hPutStrLn s "Welcome to passive!"
