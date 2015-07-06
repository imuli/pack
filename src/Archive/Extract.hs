module Archive.Extract ( extract ) where

import System.Exit
import System.FilePath
import System.IO

import Archive.Helpers
import Archive.Types

extract :: ArchiveType -> FilePath -> FilePath -> IO ExitCode
extract archivetype file dir =
    case archivetype of
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
         _ -> fail
  where
    gz cmd = runCommand dir stdin (dir ++ "/file") cmd ["-dc", file]
    dos cmd = path cmd ["x", file]
    bare cmd = path cmd [file]
    path = runCommand dir stdin stdout
    fail = error $ file ++ ": Unable to extract " ++ (show archivetype) ++ " archives."


