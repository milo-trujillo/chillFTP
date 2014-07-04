{-
	This module is responsible for converting between Haskell data types and 
	data formats used by FTP.
-}

module FTP where

import Network.Socket

-- FTP commands are defined as (Command, Argument)
type Command = (String, String)

-- Converts a string into FTP commands and arguments
makeCommand :: String -> Command
makeCommand s
	| length args == 1 = (s, "")
	| length args == 2 = (args !! 0, args !! 1)
	| otherwise = ("", "")
	where args = (words s)

-- Returns only the action part of an FTP command
getCommand :: Command -> String
getCommand (c, _) = c

-- Takes a socket address, returns it in proper FTP string format.
-- Host strings are in the format "h1,h2,h3,h4,p1,p2" as described
-- here: http://cr.yp.to/ftp/retr.html
getFTPAddr :: SockAddr -> String
getFTPAddr address = let addr = (show address) 
	in parse addr
	where 
		parse (x:xs)
			| (length xs == 0) = ""
			| ([x] == ":") = ""
			| ([x] == ".") = [','] ++ (parse xs)
			| otherwise = [x] ++ (parse xs)
		parse [] = ""

-- Takes a port number, returns it in FTP format "p1,p2" 
-- where (p1*256) + p2 == port
getFTPPort :: Int -> String
getFTPPort port = ((show p1) ++ "," ++ (show p2))
	where
		p2 = port `mod` 256		-- Equivalent to "mod port 256"
		p1 = (port - p2) `div` 256

-- Easy conversion from a port number to an integer
portToInt :: PortNumber -> Int
portToInt portno = (read (show (portno)) :: Int)
