{-
	This module is responsible for interpreting strings as FTP commands.
-}

module FTP where

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
