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
    	 A7Z 	-> path "7z"       $ ["a", dest ] ++ files
    	 ARC 	-> path "arc"      $ ["a", dest ] ++ files
    	 ARJ 	-> path "arj"      $ ["a", dest ] ++ files
         BZIP2 	-> pipe "bzip2"    $ ["-c", "--"] ++ files
    	 CAB 	-> path "lcab"	   $ ["-qr", "--"] ++ files ++ [dest]
         COMP 	-> pipe "compress" $ ["-c", "--"] ++ files
         GZIP  	-> pipe "gzip"     $ ["-c", "--"] ++ files
         KGB  	-> path "kgb"      $ dest : files
         LZIP  	-> pipe "lzip"     $ ["-c", "--"] ++ files
         LZMA  	-> pipe "lzma"     $ ["-c", "--"] ++ files
         LZOP  	-> pipe "lzop"     $ ["-c", "--"] ++ files
         TAR	-> pipe "tar"      $ ["-c"] ++ files
    	 RAR 	-> path "rar"      $ ["a", dest ] ++ files
    	 RZIP 	-> path "rzip"     $ ["-k", "-o", dest ] ++ files
         XZ	-> pipe "xz"       $ ["-c", "--"] ++ files
         ZIP	-> path "zip"      $ ["-qr", dest, "--"] ++ files
    	 ZOO 	-> path "zoo"      $ ["qa", dest ] ++ files
    	 ZPAQ 	-> path "zpaq"     $ ["qc", dest ] ++ files
         _ -> fail files
  where
    pipe = compactPipe dest
    path = compactPath
    fail _ = error $ "Attempted to compact to a file of unknown type."

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

pack :: Int -> [FileType] -> [FilePath] -> IO ()
pack _ [] _ = return ()
pack depth types absfiles = do
    ensureNoPath dest
    r <- compact filetype files dest dir
    if r == ExitSuccess
        then do removeIntermediaries depth files
                absdest <- canonicalizePath dest
                pack (depth+1) (tail types) [absdest]
        else exitWith r
  where
    prefix = commonPath absfiles
    dir = (takeDirectory prefix) ++ "/"
    files = map (removePrefix dir) absfiles
    filetype = (head types)
    dest = (ifNull flags_name $ takeFileName prefix) ++ "." ++ show filetype
    ifNull [] x = x
    ifNull x _ = x

main :: IO ()
main = do
    _ <- $initHFlags "pack 0.1\n\n\tpack [options] [files]"
    absfiles <- mapM canonicalizePath arguments
    pack 0 flags_type absfiles
