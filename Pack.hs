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

defineEQFlag "t:type" [| [TAR, XZ] :: [FileType] |] "type" "Formats"
defineEQFlag "f:force" [| False :: Bool |] "" "Force overwrite."
defineEQFlag "n:name" [| "" :: String |] "[name]" "Output Base Name"
return[]

-- | extract into a file in specifile directory
compactPath :: String -> [String] -> FilePath -> FilePath -> IO ExitCode
compactPath cmd args dest dir = do
    destH <- openBinaryFile dest WriteMode
    (_,_,_,p) <- createProcess (proc cmd args){ cwd = Just dir
                                              , close_fds = True
                                              , std_out = UseHandle destH
                                              }
    waitForProcess p

compact :: FileType -> [FilePath] -> FilePath -> FilePath -> IO ExitCode
compact filetype files =
    case filetype of
    	 7Z -> error "7z cannot compress to stdout."
    	 ARC -> error "ARC cannot compress to stdout."
    	 ARJ -> error "ARJ cannot compress to stdout."
         BZIP2 -> compactPath "bzip2" $ ["-c", "--"] ++ files
         GZIP -> compactPath "gzip" $ ["-c", "--"] ++ files
         LZMA -> compactPath "lzma" $ ["-c", "--"] ++ files
         TAR -> compactPath "tar" $ ["-c"] ++ files
         XZ -> compactPath "xz" $ ["-c", "--"] ++ files
         ZIP -> compactPath "zip" $ ["-q", "-r", "-", "--"] ++ files
         _ -> fail files
  where
    fail _ _ = error $ "Attempted to compact to a file of unknown type."

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
    prefix = commonPrefix absfiles
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
