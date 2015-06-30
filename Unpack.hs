{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Data.Maybe
import HFlags
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import System.Random

data FileType = Unknown | GZIP | BZIP2 | LZMA | XZ | TAR | ZIP
        deriving (Read, Show)

defineEQFlag "t:type" [| Unknown :: FileType |] "type" "Force file type."
defineFlag "k:keep" False "Keep original file."
defineFlag "f:force" False "Force overwrite."
defineFlag "v:verbose" False "List files as they are unpacked."
return[]

-- | extract into a directory
extractDir :: String -> [String] -> FilePath -> IO ExitCode
extractDir cmd args dest = do
    (_,_,_,p) <- createProcess (proc cmd args){ cwd = Just dest
                                              , close_fds = True
                                              }
    waitForProcess p

-- | extract into a file in specifile directory
extractFile :: String -> [String] -> FilePath -> IO ExitCode
extractFile cmd args dest = do
    destH <- openBinaryFile (dest ++ "/file") WriteMode
    (_,_,_,p) <- createProcess (proc cmd args){ cwd = Just dest
                                              , close_fds = True
                                              , std_out = UseHandle destH
                                              }
    waitForProcess p

extract :: FileType -> FilePath -> FilePath -> IO ExitCode
extract filetype file =
    case filetype of
         GZIP -> extractFile "gzip" ["-dc", "--", file]
         BZIP2 -> extractFile "bzip2" ["-dc", "--", file]
         LZMA -> extractFile "lzma" ["-dc", "--", file]
         XZ -> extractFile "xz" ["-dc", "--", file]
         TAR -> extractDir "tar" ["-xf", file]
         ZIP -> extractDir "unzip" ["--", file]
         _ -> fail
  where
    fail _ = return $ ExitFailure 1 -- FIXME

tempDir :: IO String
tempDir = do
    -- FIXME get more than 45 bits here?
    bits <- liftM (take 10 . randomRs ('a', 'z')) newStdGen
    let name = "unpack-" ++ bits in do
        createDirectory name
        return name

getFileType :: FilePath -> IO FileType
getFileType file = do
    case flags_type of
         Unknown -> return Unknown -- FIXME implement
         x -> return x

renameTo :: FilePath -> FilePath -> IO ()
renameTo dest dir = do
    files <- liftM (filter isRealEntry) (getDirectoryContents dir)
    case length files of
         0 -> removeDirectory dir
         1 -> do renameFile (dir ++ "/" ++ head files) dest
                 removeDirectory dir
         _ -> renameFile dir dest
  where
    isRealEntry name = case name of
                            "." -> False
                            ".." -> False
                            _ -> True

maybeRemoveFile :: FilePath -> IO ()
maybeRemoveFile file = do
    p <- getPermissions file
    case flags_keep || (not . writable) p of
         False -> removeFile file
         True -> return ()

checkDestination :: FilePath -> IO ()
checkDestination file = do
    exists <- doesFileExist file
    case flags_force || not exists of
         False -> exitWith $ ExitFailure 1 -- FIXME
         True -> return ()

unpack :: FilePath -> IO ()
unpack relfile = do
    file <- canonicalizePath relfile
    let dest = takeBaseName file in do
        checkDestination dest
        filetype <- getFileType file
        dir <- tempDir
        r <- extract filetype file dir
        case r of
             ExitSuccess -> do
                            renameTo dest dir
                            maybeRemoveFile file
             x -> do 
                  removeDirectoryRecursive dir
                  exitWith x

main :: IO ()
main = do
    _ <- $initHFlags "unpack 0.1"
    mapM_ unpack arguments

