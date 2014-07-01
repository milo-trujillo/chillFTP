{-
	This server is an incredibly simple TCP FTP daemon. It's probably missing
	a ton of features and might not be compatible with the full FTP standard.
-}

import System.IO				-- For handles
import Network.Socket			-- For sockets
import Control.Concurrent		-- For threads and channels
import Control.Exception		-- For exception handling

-- Global config vars
max_connections = 30
command_port = 21
data_port = 20

-- Commands are defined as (Command, Arguments)
type Command = (String, String)

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
	forkIO (handleClient client)
	listenLoop servSock

-- Handles an individual client command connection
handleClient :: (Socket, SockAddr) -> IO ()
handleClient (sock, addr) = do
	s <- socketToHandle sock ReadWriteMode	-- Convert socket to filehandle
	hSetNewlineMode s (NewlineMode { inputNL =  CRLF, outputNL = LF })
	hPutStr s "220 Server ready.\n"
	{-
		TODO: Break this into command interpretation, like:
			user <- getUser (getCommand (hGetLine s))
		Then the if statement can just compare usernames, not commands
	-}
	user <- hGetLine s
	if (user /= "USER ftp" && user /= "USER anonymous") then do
		hPutStrLn s "530 Anonymous login only."
		hClose s
	else do
		hPutStrLn s "230 Login successful.\n"
		hPutStrLn s "231 Goodbye\n"
		hClose s
