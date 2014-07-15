{-
	This module is responsible for the control port.
	It begins once a connection has been accepted in the Main module.
-}

module Control (handleClient) where

import System.IO				-- For handles
import Network.Socket			-- For sockets
import Control.Concurrent		-- For threads, channels, and MVars

import FTP						-- For interpreting FTP commands
import PASV						-- For handling the passive data connection
import Filesystem				-- For changing directory and Status

-- Handles authenticating an individual client command connection
-- Then hands off the connection to clientLoop
handleClient :: (Socket, SockAddr) -> IO ()
handleClient (sock, addr) = do
	logMsg ("New connection from " ++ show(addr))
	origin <- getSocketName sock
	logMsg ("(" ++ (getFTPAddr addr) ++ ") to " ++ (getFTPAddr origin))
	s <- socketToHandle sock ReadWriteMode
	-- FTP puts '\r\n' at the end of all lines, we need to strip it
	hSetNewlineMode s (NewlineMode { inputNL =  CRLF, outputNL = LF })
	hSetBuffering s LineBuffering
	hPutStrLn s "220 Server ready."
	line <- hGetLine s
	let (command, user) = makeCommand line
	if (command == "USER" && (user == "ftp" || user == "anonymous")) then do
		hPutStrLn s "230 Login successful."
		pasvChan <- newChan
		path <- newMVar "/"
		clientLoop s addr pasvChan path
		hClose s
	else do
		if (command == "USER") then do
			hPutStrLn s "530 Anonymous login only."
			hClose s
		else do
			hPutStrLn s "530 Not logged in."
			hClose s

-- Handles all client commands post-login
clientLoop :: Handle -> SockAddr -> Chan Request -> MVar String -> IO ()
clientLoop s addr pasv wd = do
	line <- hGetLine s
	let (command, args) = makeCommand line
	case command of
		"QUIT"	->	do
			hPutStrLn s "231 Goodbye." 
			hClose s
			callback <- newEmptyMVar
			writeChan pasv ((command, args), callback)
		"SYST"	->	hPutStrLn s "215 UNIX Type: L8"
		"FEAT"	->	do
			hPutStrLn s "211-Features:"
			hPutStrLn s " PASV"
			hPutStrLn s " SIZE"
			hPutStrLn s "211 End"
		"HELP"	->	do
			hPutStrLn s "250 To reach the system administrator, try prayer."
		-- TODO: Implement real TYPE functionality
		"TYPE"	->	do -- Stub function to make ftp clients happy
			hPutStrLn s ("200 Type set to I.")
		"PWD"	->	do
			path <- viewMVar wd
			hPutStrLn s ("257 \"" ++ path ++ "\" is current directory.")
		"CWD"	->	do
			current <- viewMVar wd
			(path, result) <- validatePath current args
			if (result == Done) then do
				status <- isValidWD path
				if (status == Done) then do
					_ <- swapMVar wd path
					hPutStrLn s "250 CWD successful."
				else handleStatus s status
			else handleStatus s result
		"NLST"	->	do
			current <- viewMVar wd
			(path, result) <- validatePath current args
			if (result == Done) then do
				listFiles s pasv command path
			else
				handleStatus s result
		"LIST"	->	do
			current <- viewMVar wd
			(path, result) <- validatePath current args
			if (result == Done) then do
				listFiles s pasv command path
			else
				handleStatus s result
		"SIZE"	->	do
			-- TODO: Disable SIZE command for ASCII transfer mode
			current <- viewMVar wd
			(path, result) <- validatePath current args
			available <- isReadableFile path
			if (result == Done && available == Done) then do
				size <- getFileSize path
				hPutStrLn s ("213 " ++ (show size))
			else do
				if (result /= Done) then handleStatus s result
				else handleStatus s available
		"RETR"	->	do
			current <- viewMVar wd
			(path, result) <- validatePath current args
			available <- isReadableFile path
			if (result == Done && available == Done) then do
				size <- getFileSize path
				hPutStr s ("150 Opening BINARY mode data connection for ")
				hPutStrLn s ("'" ++ args ++ "' (" ++ (show size) ++ " bytes).")
				callback <- newEmptyMVar
				writeChan pasv (("RETR", path), callback)
				status <- takeMVar callback
				if (status == Done) then 
					hPutStrLn s "226 Transfer complete, fools."
				else
					handleStatus s status
			else do
				if (result /= Done) then handleStatus s result
				else handleStatus s available
		-- WARNING: STOR is not finished or secure at all!
		-- Disable it or finish path validation before putting into production!
		-- TODO: Finish STOR stub
		"STOR"	->	do
			current <- viewMVar wd
			(path, result) <- validatePath current args
			available <- isWritableFile path
			-- This stuff belongs inside an if statement after validation
			hPutStr s ("150 Opening BINARY mode data connection for ")
			hPutStrLn s (args ++ ".")
			callback <- newEmptyMVar
			let path = current ++ "/" ++ args -- TEMPORARY HACK!
			writeChan pasv (("STOR", path), callback)
			status <- takeMVar callback
			if (status == Done) then
				hPutStrLn s "226 Transfer complete, fools."
			else
				handleStatus s status
		"PASV"	->	do
			let address = (getFTPAddr addr)
			callback <- newEmptyMVar
			writeChan pasv (("PASV", address), callback)
			portno <- (openPASV pasv)
			let port = (getFTPPort portno)
			hPutStr s ("227 Entering Passive Mode (" ++ address )
			hPutStrLn s ("," ++ port ++ ").")
		_		-> (hPutStrLn s "502 Command not implemented")  >>
					logMsg ("Unknown: " ++ command ++ " (" ++ args ++ ")")
	-- Now that we're done with the command, figure out if we need to read again
	streamOpen <- hIsOpen s
	if streamOpen then
		clientLoop s addr pasv wd
	else
		return ()

-- Lists files over a PASV data connection
listFiles :: Handle -> Chan Request -> String -> FilePath -> IO ()
listFiles s pasv command path = do
	hPutStrLn s "150 Opening ASCII connection for file list"
	callback <- newEmptyMVar
	writeChan pasv ((command, path), callback)
	status <- takeMVar callback
	if (status == Done) then 
		hPutStrLn s "226 Transfer complete, fools."
	else
		handleStatus s status

-- Returns the contents of an MVar without modifying it
-- WARNING: NOT GUARANTEED TO BE ATOMIC
viewMVar :: MVar a -> IO a
viewMVar var = do
	foo <- takeMVar var
	putMVar var foo
	return foo

-- Handles several regular error messages
handleStatus :: Handle -> Status -> IO ()
handleStatus s status = do
	case status of
		PermDenied	->	hPutStrLn s "550 Permission denied."
		NotFound	->	hPutStrLn s "550 File not found."
		Error	->	hPutStrLn s "550 Unexpected error."
		Done	->	return () -- This should be handled _before_ calling us
