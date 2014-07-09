{-
	This module is responsible for interactions between the filesystem and the
	FTP server. It provides several abstractions for the rest of the codebase.
-}

module Filesystem where

import Control.Concurrent		-- For MVars
import System.Directory			-- For filesystem interaction

-- Status messages report when a task is complete, or what kind of problem
-- has been encountered.
data Status = Done | PermDenied | NotFound | Error	deriving (Eq)

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

getFileList :: String -> MVar Status -> IO [String]
getFileList path result = do
	available <- isValidWD path
	if (available /= Done) then do
		putMVar result available
		return []
	else do
		names <- getDirectoryContents path
		putMVar result Done
		return names
