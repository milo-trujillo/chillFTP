{-
	This module is responsible for the control port.
	It begins once a connection has been accepted in the Main module.
-}

module Control (handleClient) where

import System.IO				-- For handles
import Network.Socket			-- For sockets
import Control.Concurrent		-- For threads and channels

import FTP						-- For interpreting FTP commands
import PASV						-- For handling the passive data connection
import Filesystem				-- For changing directory and Status

-- Handles authenticating an individual client command connection
-- Then hands off the connection to clientLoop
handleClient :: (Socket, SockAddr) -> IO ()
handleClient (sock, addr) = do
	putStr ("New connection from " ++ show(addr))
	origin <- getSocketName sock
	putStrLn ("(" ++ (getFTPAddr addr) ++ ") to " ++ (getFTPAddr origin))
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
		clientLoop s addr pasvChan
		hClose s
	else do
		if (command == "USER") then do
			hPutStrLn s "530 Anonymous login only."
			hClose s
		else do
			hPutStrLn s "530 Not logged in."
			hClose s

-- Handles all client commands post-login
clientLoop :: Handle -> SockAddr -> Chan Request -> IO ()
clientLoop s addr pasv = do
	line <- hGetLine s
	let (command, args) = makeCommand line
	case command of
		"QUIT"	->	quitClient
					where quitClient = do
						hPutStrLn s "231 Goodbye." 
						hClose s
						callback <- newEmptyMVar
						writeChan pasv ((command, args), callback)
					-- We'll hardcode the PWD now for testing
		"SYST"	->	hPutStrLn s "215 UNIX Type: L8"
		"FEAT"	->	hPutStrLn s "211-Features:" >>
					hPutStrLn s " PASV" >>
					hPutStrLn s " SIZE" >>
					hPutStrLn s "211 End"
		"PWD"	->	hPutStrLn s "257 \"/\" is current directory."
		"LIST"	->	listFiles
					where listFiles = do
						hPutStrLn s "150 Opening ASCII connection for file list"
						callback <- newEmptyMVar
						writeChan pasv ((command, args), callback)
						status <- takeMVar callback
						case status of
							Done	->	hPutStrLn s "226 Transfer complete fools."
							PermDenied	->	hPutStrLn s "550 Permission denied."
							NotFound	->	hPutStrLn s "550 Folder not found."
							Error	->	hPutStrLn s "550 Unexpected error."
		"PASV"	->	startPASV
					where startPASV = do
						let address = (getFTPAddr addr)
						callback <- newEmptyMVar
						writeChan pasv (("PASV", address), callback)
						portno <- (openPASV pasv)
						let port = (getFTPPort portno)
						hPutStr s ("227 Entering Passive Mode (" ++ address )
						hPutStrLn s ("," ++ port ++ ").")
		_		-> (hPutStrLn s "502 Command not implemented")  >>
					putStrLn ("Unknown: " ++ command ++ " (" ++ args ++ ")")
	-- Now that we're done with the command, figure out if we need to read again
	streamOpen <- hIsOpen s
	if streamOpen then
		clientLoop s addr pasv
	else
		return ()
