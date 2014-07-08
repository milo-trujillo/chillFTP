{-
	This module is responsible for managing the passive data port.

	We only export the main function, openPASV. Once a passive session is open
	all interaction between the control and data ports is done via a channel.
-}

module PASV (openPASV, Request, Status(Done, PermDenied, NotFound, Error)) where

import System.IO
import Network.Socket
import Control.Concurrent

import FTP

-- Status messages report when a task is complete, or what kind of problem
-- has been encountered.
data Status = Done | PermDenied | NotFound | Error	deriving (Eq)

-- Passive requests are defined as (ftp command, callback)
-- The callback reports to the caller the result of the request
type Request = (Command, MVar Status)

-- We open one port per user data connection, so the connection limit is 1
max_connections :: Int
max_connections = 1

-- Opens a new passive ftp connection, returns the port number
openPASV :: Chan Request -> IO Int
openPASV requests = do
	sock <- socket AF_INET Stream 0		-- Make new socket
	setSocketOption sock ReuseAddr 1	-- Make reusable listening sock
	bindSocket sock (SockAddrInet aNY_PORT iNADDR_ANY)
	listen sock max_connections
	allowed_addr <- getAddr
	putStrLn ("Allowing connections from " ++ allowed_addr)
	_ <- forkIO (acceptPASV sock allowed_addr requests)
	portno <- socketPort sock
	return (read (show (portno)) :: Int)
	where
		-- We need to get the address connections are allowed from
		-- It's supplied over the channel like ("PASV", "10,0,0,2")
		getAddr = do
			request <- readChan requests
			let ((action, arg), _) = request
			if (action == "PASV") then
				return arg
			else
				getAddr

-- Accepts a connection, if it's from the allowed address
-- Runs recursively until success
acceptPASV :: Socket -> String -> Chan Request -> IO ()
acceptPASV data_listen allowed requests = do
	(sock, address) <- Network.Socket.accept data_listen
	s <- socketToHandle sock ReadWriteMode
	hSetNewlineMode s (NewlineMode { inputNL = CRLF, outputNL = CRLF })
	let addr = getFTPAddr address
	putStrLn ("Opened PASV from " ++ addr)	-- DEBUG
	if (addr == allowed) then do
		handlePASV s requests
	else do
		hPutStrLn s "Access denied."
		hClose s
		acceptPASV data_listen allowed requests

-- Handles a passive ftp connection once a user connects
handlePASV :: Handle -> Chan Request -> IO ()
handlePASV s requests = do
	((command, _), callback) <- readChan requests
	putStrLn ("Read command: " ++ command)	-- DEBUG
	case command of
		"LIST"	->	hPutStrLn s "... No files found ..." >>
					putMVar callback Done
		"QUIT"	->	putMVar callback Done
		_		->	hPutStrLn s "ERROR: Unknown command passed to PASV!" >>
					putMVar callback Error
	hClose s
