{-
	This module is responsible for interactions between the filesystem and the
	FTP server. It provides several abstractions for the rest of the codebase.
-}

module Filesystem where

import System.Directory
import Filesystem.Path.CurrentOS

-- Status messages report when a task is complete, or what kind of problem
-- has been encountered.
data Status = Done | PermDenied | NotFound | Error	deriving (Eq)

isValidWD :: String -> IO Bool
isValidWD stringPath = do
	path <- pack stringPath -- System.Directory takes Text, not Strings
	folderExists <- isDirectory path
	if (folderExists) then
		perms <- getPermissions
		if (searchable perms) then
			return True
		else
			return False
	else
		return False
