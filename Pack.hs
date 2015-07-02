{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception
import Control.Monad
import HFlags
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

import FileType
import Path

defineEQFlag "t:type" [| [TAR, XZ] :: [FileType] |] "type" "Formats"
defineEQFlag "f:force" [| False :: Bool |] "" "Force overwrite."
defineEQFlag "n:name" [| "" :: String |] "[name]" "Output Base Name"
return[]

-- | programs that require a filename
compactPath :: String -> [String] -> FilePath -> IO ExitCode
compactPath cmd args dir = do
    (_,_,_,p) <- createProcess (proc cmd args){ cwd = Just dir
                                              , close_fds = True
                                              }
    waitForProcess p

-- | programs that compress to standard output
compactPipe :: FilePath -> String -> [String] -> FilePath -> IO ExitCode
compactPipe dest cmd args dir = do
    destH <- openBinaryFile dest WriteMode
    (_,_,_,p) <- createProcess (proc cmd args){ cwd = Just dir
                                              , close_fds = True
                                              , std_out = UseHandle destH
                                              }
    waitForProcess p

compact :: FileType -> [FilePath] -> FilePath -> FilePath -> IO ExitCode
compact filetype files dest =
    case filetype of
         AR    -> path "ar"       $ ["rc", dest] ++ files
         A7Z   -> path "7z"       $ ["a", "-r", dest] ++ files
         ARC   -> dos  "arc"
         ARJ   -> dos  "arj"
         BZIP2 -> gz   "bzip2"
         CAB   -> path "lcab"     $ ["-r"] ++ files ++ [dest]
         COMP  -> gz   "compress"
         GZIP  -> gz   "gzip"
         KGB   -> path "kgb"      $ dest : files
         LRZIP -> path "lrzip"    $ ["-o", dest] ++ files
         LZIP  -> gz   "lzip"
         LZMA  -> gz   "lzma"
         LZOP  -> gz   "lzop"
         RAR   -> dos  "rar"
         RZIP  -> path "rzip"     $ ["-k", "-o", dest ] ++ files
         TAR   -> gz   "tar"
         XZ    -> gz   "xz"
         ZIP   -> path "zip"      $ ["-r", dest] ++ files
         ZOO   -> dos  "zoo"
         ZPAQ  -> dos  "zpaq"
         _ -> fail files
  where
    gz cmd = compactPipe dest cmd $ ["-c"] ++ files
    dos cmd = compactPath cmd $ ["a", dest ] ++ files
    path = compactPath
    fail _ = error $ "Unable to compress " ++ show filetype ++ "."

removePrefix :: FilePath -> FilePath -> FilePath
removePrefix common path = drop (length common) path

removeIntermediaries :: Int -> [FilePath] -> IO ()
removeIntermediaries 0 _ = return ()
removeIntermediaries _ files = mapM_ purgePath files

ensureNoPath :: FilePath -> IO ()
ensureNoPath dest = do
    exists <- doesPathExist dest
    case (flags_force, exists) of
        (False, True) -> error $ dest ++ ": already exists."
        (True, True) -> purgePath dest
        (_, False) -> return ()

pack :: Int -> [FileType] -> FilePath -> [FilePath] -> IO ()
pack _ [] _ _ = return ()
pack depth types basename absfiles = do
    ensureNoPath dest
    absdest <- absolutePath dest
    r <- compact filetype files absdest dir
    if r == ExitSuccess
        then do removeIntermediaries depth files
                pack (depth+1) (tail types) dest [absdest]
        else exitWith r
  where
    prefix = commonPath absfiles
    dir = (takeDirectory prefix) ++ "/"
    files = map (removePrefix dir) absfiles
    filetype = (head types)
    dest = basename ++ "." ++ show filetype

main :: IO ()
main = do
    _ <- $initHFlags "pack 0.1\n\n\tpack [options] [files]"
    absfiles <- mapM absolutePath arguments
    pack 0 flags_type (basename $ commonPath absfiles) absfiles
  where
    basename path = ifNull flags_name $ takeFileName path
    ifNull [] x = x
    ifNull x _ = x
