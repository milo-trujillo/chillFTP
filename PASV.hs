{-
	This module is responsible for managing the passive data port.

	We only export the main function, openPASV, all other code is private.
	Interaction between the control and data ports is done via a channel.
-}

module PASV (openPASV) where

import System.IO
import Network.Socket
import Control.Concurrent

import FTP

-- We open one port per user data connection, so the connection limit is 1
max_connections :: Int
max_connections = 1

-- Opens a new passive ftp connection, returns the port number
openPASV :: Chan Command -> IO Int
openPASV commands = do
	sock <- socket AF_INET Stream 0		-- Make new socket
	setSocketOption sock ReuseAddr 1	-- Make reusable listening sock
	bindSocket sock (SockAddrInet aNY_PORT iNADDR_ANY)
	listen sock max_connections
	allowed_addr <- getAddr
	putStrLn ("Allowing connections from " ++ allowed_addr)
	_ <- forkIO (acceptPASV sock allowed_addr commands)
	portno <- socketPort sock
	return (read (show (portno)) :: Int)
	where
		-- We need to get the address connections are allowed from
		-- It's supplied over the channel like ("PASV", "10,0,0,2")
		getAddr = do
			command <- readChan commands
			let (action, arg) = command
			if (action == "PASV") then
				return arg
			else
				getAddr

-- Accepts a connection, if it's from the allowed address
-- Runs recursively until success
acceptPASV :: Socket -> String -> Chan Command -> IO ()
acceptPASV data_listen allowed commands = do
	(sock, address) <- Network.Socket.accept data_listen
	s <- socketToHandle sock ReadWriteMode
	hSetNewlineMode s (NewlineMode { inputNL = CRLF, outputNL = LF })
	hSetBuffering s NoBuffering
	let addr = getFTPAddr address
	putStrLn ("Opened PASV from " ++ addr)	-- DEBUG
	if (addr == allowed) then do
		handlePASV s commands
	else do
		hPutStrLn s "Access denied."
		hClose s
		acceptPASV data_listen allowed commands

-- Handles a passive ftp connection once a user connects
handlePASV :: Handle -> Chan Command -> IO ()
handlePASV s commands = do
	(command, _) <- readChan commands
	putStrLn ("Read command: " ++ command)	-- DEBUG
	socketOpen <- hIsWritable s -- DEBUG
	putStrLn ("Socket is ready for writing: " ++ show(socketOpen)) -- DEBUG
	case command of
		"LIST"	->	hPutStrLn s "150 Opening ASCII connection for file list" >>
					hPutStrLn s "... No files found ..." >>
					hPutStrLn s "226 Transfer complete fools."
		"QUIT"	->	hClose s
		_		->	hPutStrLn s "502 Command not supported in PASV"
	streamOpen <- hIsOpen s
	if streamOpen then
		handlePASV s commands
	else
		return ()
