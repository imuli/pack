{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Maybe
import HFlags
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Random

import Archive
import Path

defineEQFlag "t:type" [| [Format Unknown] :: [Format] |] "type" "Formats."
defineFlag "k:keep" False "Keep original file."
return[]

tempDir :: IO String
tempDir = do
    -- FIXME get more than 45 bits here?
    bits <- liftM (take 10 . randomRs ('a', 'z')) newStdGen
    let name = "unpack-" ++ bits in do
        createDirectory name
        return name

getFileType :: [Format] -> FilePath -> IO Format
getFileType types file = do
    case types of
         [] -> return $ Format Unknown
         [Format Unknown] -> identify file
         x:xs -> return x

renameClever :: FilePath -> FilePath -> IO ()
renameClever dir dest = do
    purgePath dest
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

unpack :: Int -> [Format] -> FilePath -> IO ()
unpack depth types file = do
    filetype <- getFileType types file
    doChecks filetype
    bracket tempDir purgePath ( \dir -> do
        r <- extract filetype file dir
	case r of 
	    Left _ -> do maybeRemoveFile depth file
			 renameClever dir dest
			 unpack (depth+1) nexttypes =<< absolutePath dest
            Right _ -> exitWith $ ExitFailure 1
        )
  where
    dest = takeBaseName file
    nexttypes = case types of
        [] -> []
        [Format Unknown] -> [Format Unknown]
        x:xs -> xs
    doChecks filetype = do
        case (depth, filetype) of
             (0, Format Unknown) -> error $ file ++ ": Unknown Format."
             (_, Format Unknown) -> exitWith ExitSuccess 
             (_, _) -> freePath dest (return ())

main :: IO ()
main = do
    _ <- $initHFlags "unpack 0.1\n\n\tunpack [options] [files]"
    files <- mapM absolutePath arguments
    mapM_ (unpack 0 (reverse flags_type)) files

