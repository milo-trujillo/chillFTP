{-
	This server is an incredibly simple TCP FTP daemon. It's probably missing
	a ton of features and might not be compatible with the full FTP standard.
-}

import System.IO				-- For handles
import Network.Socket			-- For sockets
import Control.Concurrent		-- For threads and channels

import FTP						-- For interpreting FTP commands
import PASV						-- For handling the passive data connection

-- Global config vars
max_connections	:: Int
command_port	:: PortNumber
max_connections	= 30
command_port	= 21

-- Does some initial setup, eventually folds into listenLoop
main :: IO ()
main = do
	sock <- socket AF_INET Stream 0		-- Make new socket
	setSocketOption sock ReuseAddr 1	-- Make reusable listening sock
	bindSocket sock (SockAddrInet command_port iNADDR_ANY)
	listen sock max_connections			-- Set max connections
	listenLoop sock

-- Listens for client, forks off handler
listenLoop :: Socket -> IO ()
listenLoop servSock = do
	client <- Network.Socket.accept servSock
	_ <- forkIO (handleClient client)
	listenLoop servSock

-- Handles authenticating an individual client command connection
-- Then hands off the connection to clientLoop
handleClient :: (Socket, SockAddr) -> IO ()
handleClient (sock, addr) = do
	putStr ("New connection from " ++ show(addr))
	origin <- getSocketName sock
	putStrLn (" to " ++ (getFTPAddr origin))
	s <- socketToHandle sock ReadWriteMode
	-- FTP puts '\r\n' at the end of all lines, we need to strip it
	hSetNewlineMode s (NewlineMode { inputNL =  CRLF, outputNL = LF })
	hPutStrLn s "220 Server ready."
	line <- hGetLine s
	let (command, user) = makeCommand line
	if (command == "USER" && (user == "ftp" || user == "anonymous")) then do
		hPutStrLn s "230 Login successful."
		clientLoop s addr
		hClose s
	else do
		if (command == "USER") then do
			hPutStrLn s "530 Anonymous login only."
			hClose s
		else do
			hPutStrLn s "530 Not logged in."
			hClose s

-- Handles all client commands post-login
clientLoop :: Handle -> SockAddr -> IO ()
clientLoop s addr = do
	line <- hGetLine s
	let (command, args) = makeCommand line
	case command of
		"QUIT"	->	hPutStrLn s "231 Goodbye." >> hClose s
		"SYST"	->	hPutStrLn s "215 UNIX Type: L8"
		"FEAT"	->	hPutStrLn s "211-Features:" >>
					hPutStrLn s " PASV" >>
					hPutStrLn s " SIZE" >>
					hPutStrLn s "211 End"
		"PASV"	->	startPASV
					where startPASV = do
						portno <- openPASV
						let port = (getFTPPort portno)
						let address = (getFTPAddr addr)
						hPutStrLn s ("227 =" ++ address ++ "," ++ port)
		_		-> (hPutStrLn s "502 Command not implemented")  >>
					putStrLn ("Unknown: " ++ command ++ " (" ++ args ++ ")")
	-- Now that we're done with the command, figure out if we need to read again
	streamOpen <- hIsOpen s
	if streamOpen then
		clientLoop s addr
	else
		return ()
