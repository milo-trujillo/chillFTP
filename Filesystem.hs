{-
	This module is responsible for interactions between the filesystem and the
	FTP server. It provides several abstractions for the rest of the codebase.
-}

module Filesystem where

import System.Posix.Syslog (syslog, Priority(Notice))		-- For logging
import System.Posix							-- For fileSize and getFileStatus
import System.IO							-- For filehandles
--import System.IO.Error						-- For IO errors (like perm denied)
import System.Directory						-- For filesystem interaction
import qualified Data.ByteString.Lazy as BL	-- For moving binary data
import Control.Exception					-- Handling exceptions

-- Status messages report when a task is complete, or what kind of problem
-- has been encountered.
data Status = Done | PermDenied | NotFound | Error	deriving (Eq)

-- Checks if a file exists and is readable
-- Returns Error if special file (like directory)
isReadableFile :: String -> IO Status
isReadableFile path = do
	isFile <- doesFileExist path
	if (isFile) then do
		perms <- getPermissions path
		if (readable perms) then
			return Done
		else
			return PermDenied
	else do
		isDir <- doesDirectoryExist path
		if (isDir) then
			return Error -- It's a folder, not a file
		else
			return NotFound

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

-- Returns size in bytes of a file
getFileSize :: String -> IO FileOffset
getFileSize path = do
	stat <- getFileStatus path
	return (fileSize stat)

-- Returns an "rwx" style string representation of file permissions
getFilePerms :: FilePath -> IO String
getFilePerms path = do
	perms <- catch (getPermissions path)
		(\e -> do
			let err = show (e :: IOException)
			logMsg ("Error: " ++ err)
			return emptyPermissions)
	let permString = ((canRead perms) ++ (canWrite perms) ++ (canExec perms)) 
	return permString
	where
		canRead perms
			| (readable perms) = "r"
			| otherwise = "-"
		canWrite perms
			| (writable perms) = "w"
			| otherwise = "-"
		canExec perms
			| (executable perms || searchable perms) = "x"
			| otherwise = "-"

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

-- Copies from one file handle to another using lazy bytestrings
copyData :: Handle -> Handle -> IO ()
copyData fromFile toFile = do
	contents <- BL.hGetContents fromFile
	BL.hPut toFile contents
	hFlush toFile
	-- TODO: Make exception-safe with something like the following:
	--handle (\ _ -> logMsg "Error reading or writing file") (BL.hPut toFile contents)
	--handle (\ _ -> logMsg "Error flushing after data copy") (hFlush toFile)

-- Shorthand for logging and debugging through the codebase
logMsg :: String -> IO ()
logMsg msg = syslog Notice msg
