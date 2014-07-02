{-
	This module handles passive data connections for FTP
-}

module PASV where

import System.IO
import Network.Socket
import Control.Concurrent

-- Opens a new passive ftp connection, returns the port number
openPASV :: IO Int
openPASV = do
	sock <- socket AF_INET Stream 0		-- Make new socket
	setSocketOption sock ReuseAddr 1	-- Make reusable listening sock
	bindSocket sock (SockAddrInet aNY_PORT iNADDR_ANY)
	portno <- socketPort sock
	_ <- forkIO (handlePASV sock)
	return (read (show (portno)) :: Int)

-- Handles a passive ftp connection once a user connects
handlePASV :: Socket -> IO ()
handlePASV pasv = do
	(sock, _) <- Network.Socket.accept pasv
	s <- socketToHandle sock ReadWriteMode
	hPutStrLn s "Welcome to passive!"
