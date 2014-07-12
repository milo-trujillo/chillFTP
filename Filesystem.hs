{-
	This module is responsible for interactions between the filesystem and the
	FTP server. It provides several abstractions for the rest of the codebase.
-}

module Filesystem where

import System.Posix.Syslog (syslog, Priority(Notice))		-- For logging
import System.Directory			-- For filesystem interaction

-- Status messages report when a task is complete, or what kind of problem
-- has been encountered.
data Status = Done | PermDenied | NotFound | Error	deriving (Eq)

-- Checks if a directory exists and can be searched through
isValidWD :: String -> IO Status
isValidWD path = do
	folderExists <- doesDirectoryExist path
	if (folderExists) then do
		perms <- getPermissions path
		if (searchable perms) then
			return Done
		else
			return PermDenied
	else
		return NotFound

-- Returns a list of files (if possible), and a status indicating any problem
getFileList :: String -> IO ([String], Status)
getFileList path = do
	available <- isValidWD path
	if (available /= Done) then do
		return ([], available)
	else do
		names <- getDirectoryContents path
		return (names, Done)

-- Given a working directory and a path, returns a simplified path
-- The file or directory is also guaranteed to exist, but _not_ be be accessible
validatePath :: String -> String -> IO (FilePath, Status)
validatePath wd p
	| (length p == 0) = return (wd, Done)
	| (p !! 0 == '/') = (simplify ("." ++ p))
	| otherwise = (simplify (wd ++ "/" ++ p))
	where simplify s = do
		fExists <- doesFileExist s
		dExists <- doesDirectoryExist s
		if( fExists || dExists ) then do
			path <- canonicalizePath s
			return (path, Done)
		else
			return (wd, NotFound)

-- Shorthand for logging and debugging through the codebase
logMsg :: String -> IO ()
logMsg msg = syslog Notice msg
