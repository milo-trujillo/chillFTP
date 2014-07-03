{-
	This module is responsible for interpreting strings as FTP commands.
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
