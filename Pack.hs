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

import Path
import Archive

defineEQFlag "t:type" [| [TAR, XZ] :: [ArchiveType] |] "type" "Formats"
defineEQFlag "f:force" [| False :: Bool |] "" "Force overwrite."
defineEQFlag "n:name" [| "" :: String |] "[name]" "Output Base Name"
return[]

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

pack :: Int -> [ArchiveType] -> FilePath -> [FilePath] -> IO ()
pack _ [] _ _ = return ()
pack depth types basename absfiles = do
    ensureNoPath dest
    absdest <- absolutePath dest
    r <- build filetype files absdest dir
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
