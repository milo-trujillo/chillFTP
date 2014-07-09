{-
	This server is a stripped down ftpd, supporting only passive mode.
	It is intended as an exercise rather than production code.

	This module is responsible for daemonization, chrooting, and listening.
-}

module Main (main) where

import Network.Socket			-- For sockets
import Control.Concurrent		-- For threads and channels

import Control					-- For handleClient

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
