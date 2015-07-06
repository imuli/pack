{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception
import Control.Monad
import HFlags
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import Archive
import Path

defineEQFlag "t:type" [| [Format Tar, Format Xz] :: [Format] |] "type" "Formats"
defineEQFlag "n:name" [| "" :: String |] "[name]" "Output Base Name"
return[]

removePrefix :: FilePath -> FilePath -> FilePath
removePrefix common path = drop (length common) path

removeIntermediaries :: Int -> [FilePath] -> IO ()
removeIntermediaries 0 _ = return ()
removeIntermediaries _ files = mapM_ purgePath files

pack :: Int -> [Format] -> FilePath -> [FilePath] -> IO ()
pack _ [] _ _ = return ()
pack depth types basename absfiles = do
    freePath dest (purgePath dest)
    absdest <- absolutePath dest
    r <- build filetype files absdest dir
    case r of
        Left _ -> do removeIntermediaries depth files
                     pack (depth+1) (tail types) dest [absdest]
        Right m -> do hPutStrLn stderr m
	              exitWith $ ExitFailure 1
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
