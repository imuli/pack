module Archive.Build ( build ) where

import System.FilePath
import System.Exit
import System.IO

import Archive.Helpers
import Archive.Types

build :: ArchiveType -> [FilePath] -> FilePath -> FilePath -> IO ExitCode
build filetype files archive dir =
    case filetype of
         AR    -> path "ar"       $ ["rc", archive] ++ files
         A7Z   -> path "7z"       $ ["a", "-r", archive] ++ files
         ARC   -> dos  "arc"
         ARJ   -> dos  "arj"
         BZIP2 -> gz   "bzip2"
         CAB   -> path "lcab"     $ ["-r"] ++ files ++ [archive]
         COMP  -> gz   "compress"
         GZIP  -> gz   "gzip"
         KGB   -> path "kgb"      $ archive : files
         LRZIP -> path "lrzip"    $ ["-o", archive] ++ files
         LZIP  -> gz   "lzip"
         LZMA  -> gz   "lzma"
         LZOP  -> gz   "lzop"
         RAR   -> dos  "rar"
         RZIP  -> path "rzip"     $ ["-k", "-o", archive ] ++ files
         TAR   -> gz   "tar"
         XZ    -> gz   "xz"
         ZIP   -> path "zip"      $ ["-r", archive] ++ files
         ZOO   -> dos  "zoo"
         ZPAQ  -> dos  "zpaq"
         _ -> fail files
  where
    gz cmd = runCommand dir stdin archive cmd $ ["-c"] ++ files
    dos cmd = runCommand dir stdin stdout cmd $ ["a", archive ] ++ files
    path = runCommand dir stdin stdout
    fail _ = error $ "Unable to compress " ++ show filetype ++ "."

