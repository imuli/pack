{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Char
import Data.Maybe
import HFlags
import Magic
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import System.Random
import Text.Read
import Text.ParserCombinators.ReadP (string, munch)

data FileType = Unknown | GZIP | BZIP2 | LZMA | XZ | TAR | ZIP
        deriving (Show, Enum)

instance Read FileType where
    readPrec = (choice $ map strVal [ ("octet-stream", Unknown)
                                    , ("bzip2", BZIP2)
                                    , ("gzip", GZIP)
                                    , ("lzma", LZMA)
                                    , ("xz", XZ)
                                    , ("tar", TAR)
                                    , ("zip", ZIP)
                                    ]) <++ allElse Unknown
      where
        strVal (x, y) = lift $ string x >> return y
        allElse y = lift $ munch (\_ -> True) >> return y

normFiletype :: String -> String
normFiletype filetype = case map toLower filetype of
    ('a':'p':'p':'l':'i':'c':'a':'t':'i':'o':'n':'/':xs) -> normFiletype xs
    ('x':'-':xs) -> normFiletype xs
    x -> x

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
         ZIP -> extractDir "unzip" ["-q", "--", file]
         _ -> fail file
  where
    fail file _ = error $ file ++ ": Attempted to extract a file of unknown type."

tempDir :: IO String
tempDir = do
    -- FIXME get more than 45 bits here?
    bits <- liftM (take 10 . randomRs ('a', 'z')) newStdGen
    let name = "unpack-" ++ bits in do
        createDirectory name
        return name

getFileType :: Magic -> FilePath -> IO FileType
getFileType magic file = do
    case flags_type of
         Unknown -> do
                    mimetype <- magicFile magic file
                    return (read $ normFiletype mimetype :: FileType)
         x -> return x

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

maybeRemoveFile :: FilePath -> IO ()
maybeRemoveFile file = do
    p <- getPermissions file
    case flags_keep || (not . writable) p of
         False -> removeFile file
         True -> return ()

checkDestination :: FilePath -> IO ()
checkDestination file = do
    isfile <- doesFileExist file
    isdir <- doesDirectoryExist file
    case flags_force || not (isfile || isdir) of
         False -> error $ file ++ ": Already Exists."
         True -> return ()

renamePath :: FilePath -> FilePath -> IO ()
renamePath path dest = do
    isfile <- doesFileExist path
    isdir <- doesDirectoryExist path
    case (isfile, isdir) of
        (True, False) -> renameFile path dest
        (False, True) -> renameDirectory path dest
        _ -> return ()

purgePath :: FilePath -> IO ()
purgePath path = do
    isfile <- doesFileExist path
    isdir <- doesDirectoryExist path
    case (isfile, isdir) of
        (True, False) -> removeFile path
        (False, True) -> removeDirectoryRecursive path
        _ -> return ()

unpack :: Magic -> FilePath -> IO ()
unpack magic relfile = do
    file <- canonicalizePath relfile
    let dest = takeBaseName file in do
        checkDestination dest
        filetype <- getFileType magic file
        bracket tempDir purgePath ( \dir -> do
            r <- extract filetype file dir
            case r of
                 ExitSuccess -> do
                                renameClever dir dest
                                maybeRemoveFile file
                 x -> do 
                      exitWith x
            )

main :: IO ()
main = do
    _ <- $initHFlags "unpack 0.1"
    magic <- magicOpen [MagicMimeType]
    magicLoadDefault magic
    mapM_ (unpack magic) arguments

