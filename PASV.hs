{-
	This module is responsible for managing the passive data port.

	We only export the main function, openPASV. Once a passive session is open
	all interaction between the control and data ports is done via a channel.
-}

module PASV (openPASV, Request) where

import System.IO			-- For handles
import Network.Socket		-- For IO
import Control.Concurrent	-- For Threads, Channels, and MVars
import Control.Monad		-- For forM_
import Text.Printf			-- For hPrintf

import FTP			-- For commands
import Filesystem	-- For file IO and Status

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
	logMsg ("Allowing connections from " ++ allowed_addr)
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
	logMsg ("Opened PASV from " ++ addr)	-- DEBUG
	if (addr == allowed) then do
		handlePASV s requests
	else do
		hPutStrLn s "Access denied."
		hClose s
		acceptPASV data_listen allowed requests

-- Handles a passive ftp connection once a user connects
handlePASV :: Handle -> Chan Request -> IO ()
handlePASV s requests = do
	((command, args), callback) <- readChan requests
	logMsg ("Read command: " ++ command)	-- DEBUG
	case command of
		"NLST"	->	do
			(names, result) <- getFileList args
			if (result == Done) then do
				forM_ names (hPutStrLn s)
				putMVar callback Done
			else
				putMVar callback result
		"LIST"	->	do
			(names, result) <- getFileList args
			if (result == Done) then do
				forM_ names (displayPerms s args)
				putMVar callback Done
			else
				putMVar callback result
		"RETR"	->	do
			--(file, result) <- getFileHandle args ReadMode
			-- Next two lines a hack until getFileHandle is finished
			file <- openFile args ReadMode
			let result = Done
			if (result == Done) then do
				copyData file s
				putMVar callback Done
			else
				putMVar callback result
		"STOR"	->	do
			--(file, result) <- getFileHandle args WriteMode
			-- Next two lines a hack until getFileHandle is finished
			file <- openFile args WriteMode
			let result = Done
			if (result == Done) then do
				copyData s file
				putMVar callback Done
			else
				putMVar callback result
		"QUIT"	->	putMVar callback Done
		_		->	hPutStrLn s "ERROR: Unknown command passed to PASV!" >>
					putMVar callback Error
	hClose s

-- Displays file permissions and file name like "rwx            foo"
displayPerms :: Handle -> FilePath -> FilePath -> IO ()
displayPerms s path file = do
	perms <- getFilePerms (path ++ "/" ++ file)
	hPrintf s "%-20s%s\n" perms file
