{-
	This server is a stripped down ftpd, supporting only passive mode.
	It is intended as an exercise rather than production code.

	This module is responsible for daemonization and listening.
-}

module Main (main) where

import System.Posix.Daemonize (CreateDaemon(..), serviced)	-- For daemonizing
import Network.Socket										-- For sockets
import Control.Concurrent									-- Threads, channels

-- Needed for debugging only
import System.Posix.User ( groupID, setGroupID, getGroupEntryForName
                         , userID, setUserID, getUserEntryForName )

import Control												-- For handleClient

-- Global config vars
max_connections	:: Int
command_port	:: PortNumber
max_connections	= 30
command_port	= 21

-- Does some initial setup, eventually folds into listenLoop
main :: IO ()
-- For release
--main = serviced ftpServer	-- Start us up as a Unix service
-- For debug
main = do
	sock <- bindServer
	getGroupEntryForName "nobody" >>= setGroupID . groupID
	getUserEntryForName "_ftp" >>= setUserID . userID
	listenLoop sock


ftpServer :: CreateDaemon Socket
ftpServer = CreateDaemon {
	privilegedAction = bindServer,	-- The result of bindServer
	program = listenLoop,			-- is the argument to listenLoop
	name = Just "chillFTP",
	user = Just "_ftp",
	group = Just "nobody",
	syslogOptions = [],
	pidfileDirectory = Nothing -- Default is /var/run
}

-- Binds a socket to the listening port, then returns the socket
bindServer :: IO Socket
bindServer = do
	sock <- socket AF_INET Stream 0		-- Make new socket
	setSocketOption sock ReuseAddr 1	-- Make reusable listening sock
	bindSocket sock (SockAddrInet command_port iNADDR_ANY)
	listen sock max_connections			-- Set max connections
	return sock

-- Listens for client, forks off handler
listenLoop :: Socket -> IO ()
listenLoop servSock = do
	client <- Network.Socket.accept servSock
	_ <- forkIO (handleClient client)
	listenLoop servSock
