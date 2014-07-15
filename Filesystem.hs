{-
	This module is responsible for interactions between the filesystem and the
	FTP server. It provides several abstractions for the rest of the codebase.
-}

module Filesystem where

import System.Posix.Syslog (syslog, Priority(Notice))		-- For logging
import System.Posix							-- For fileSize and getFileStatus
import System.IO							-- For filehandles
import System.Directory						-- For filesystem interaction
import Data.List							-- For sort
import Data.Maybe							-- For Just/Maybe IO operations
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
		perms <- catch (getPermissions path)
			(\e -> do
				let err = show (e :: IOException)
				logMsg ("Error: " ++ err)
				return emptyPermissions)
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

-- Checks if a path does not exist, and the folder is writable
isWritableFile :: String -> IO Status
isWritableFile path = do
	isFile <- doesFileExist path
	isDir <- doesDirectoryExist path
	-- We never let the client overwrite files, too insecure
	if( isFile || isDir ) then return PermDenied
	else do
		-- TODO: Verify that the enclosing folder is writable
		-- This stub is good enough for debugging, but needs finishing
		return Done

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

-- Returns a handle to a file if possible
-- Substantially more reliable than a raw openFile call
{-
getFileHandle :: FilePath -> IOMode -> (Handle, Status)
getFileHandle path fileMode = do
	file <- catch (Just openFile path fileMode)
		(\e -> do
			let err = show (e :: IOException)
			logMsg ("Error: " ++ err)
			return Nothing)
	if (file /= Nothing) then
		return (Just file, Done)
	else do
		isFile <- doesFileExist path
		isDir <- doesDirectoryExist path
		if (isFile || isDir) then
			return (Just file, PermDenied) -- Handle must not be allowed
		else
			return (Just file, Error) -- Who knows, path invalid or something
-}


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
		return ((sort names), Done)

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
	catch (BL.hPut toFile contents)
		(\e -> do
			let err = show (e :: IOException)
			logMsg ("Error reading or writing file: " ++ err)
			return ())
	catch (hFlush toFile)
		(\e -> do
			let err = show (e :: IOException)
			logMsg ("Error flushing after data transfer: " ++ err)
			return ())

-- Shorthand for logging and debugging through the codebase
logMsg :: String -> IO ()
logMsg msg = syslog Notice msg
