{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Maybe
import HFlags
import Magic
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import System.Random

import FileType
import Path

defineEQFlag "t:type" [| [Unknown] :: [FileType] |] "type" "Formats."
defineFlag "k:keep" False "Keep original file."
defineFlag "f:force" False "Force overwrite."
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
         A7Z   -> dos  "7z"
         ACE   -> dos  "unace"
         ADF   -> bare "unadf"
         ALZ   -> bare "unalz"
         AR    -> dos  "ar"
         ARC   -> dos  "arc"
         ARJ   -> dos  "arj"
         BZIP2 -> gz   "bzip2"
         CAB   -> bare "cabextract"
         COMP  -> gz   "compress"
         DEB   -> path "dpkg-deb" ["-x", file, "."]
         DMS   -> dos  "xdms"
         GZIP  -> gz   "gzip"
         KGB   -> bare "kgb"
         LHA   -> dos  "lha"
         LRZIP -> path "lrzip" ["-d", "-q", "-o", "file", file]
         LZIP  -> gz   "lzip"
         LZMA  -> gz   "lzma"
         LZOP  -> gz   "lzop"
         RAR   -> dos  "unrar"
         RZIP  -> path "rzip"  ["-d", "-k", "-o", "file", file]
         TAR   -> path "tar"   ["-xf", file]
         XZ    -> gz   "xz"
         ZIP   -> bare "unzip"
         ZOO   -> dos  "zoo"
         ZPAQ  -> dos  "zpaq"
         _ -> fail file
  where
    gz cmd = extractFile cmd ["-dc", file]
    dos cmd = extractDir cmd ["x", file]
    bare cmd = extractDir cmd [file]
    path = extractDir
    fail file _ = error $ file ++ ": Attempted to extract a file of unknown type."

tempDir :: IO String
tempDir = do
    -- FIXME get more than 45 bits here?
    bits <- liftM (take 10 . randomRs ('a', 'z')) newStdGen
    let name = "unpack-" ++ bits in do
        createDirectory name
        return name

getFileType :: [FileType] -> Magic -> FilePath -> IO FileType
getFileType types magic file = do
    case types of
         [] -> return Unknown
         [Unknown] -> do
                    mimetype <- magicFile magic file
                    return (read mimetype :: FileType)
         x:xs -> return x

renameClever :: FilePath -> FilePath -> IO ()
renameClever dir dest = do
    files <- liftM (filter isRealEntry) (getDirectoryContents dir)
    case length files of
         0 -> return ()
         1 -> renamePath (dir ++ "/" ++ head files) dest
         _ -> renameDirectory dir dest
  where
    isRealEntry name = case name of
                            "." -> False
                            ".." -> False
                            _ -> True

maybeRemoveFile :: Int -> FilePath -> IO ()
maybeRemoveFile depth file = do
    p <- getPermissions file
    case depth == 0 && (flags_keep || (not . writable) p) of
         False -> removeFile file
         True -> return ()

unpack :: Int -> [FileType] -> Magic -> FilePath -> IO ()
unpack depth types magic file = do
    filetype <- getFileType types magic file
    doChecks filetype
    bracket tempDir purgePath ( \dir -> do
        r <- extract filetype file dir
        if r == ExitSuccess
            then do maybeRemoveFile depth file
                    renameClever dir dest
                    unpack (depth+1) nexttypes magic =<< absolutePath dest
            else exitWith r
        )
  where
    dest = takeBaseName file
    nexttypes = case types of
        [] -> []
        [Unknown] -> [Unknown]
        x:xs -> xs
    doChecks filetype = do
        exists <- doesPathExist dest
        case (depth, not flags_force && exists, filetype) of
             (0, True, _) -> error $ file ++ ": Already Exists."
             (0, _, Unknown) -> error $ file ++ ": Unknown Format."
             (_, True, _) -> exitWith ExitSuccess
             (_, _, Unknown) -> exitWith ExitSuccess 
             (_, False, _) -> return ()


main :: IO ()
main = do
    _ <- $initHFlags "unpack 0.1\n\n\tunpack [options] [files]"
    magic <- magicOpen [MagicMimeType]
    magicLoadDefault magic
    files <- mapM absolutePath arguments
    mapM_ (unpack 0 (reverse flags_type) magic) files

