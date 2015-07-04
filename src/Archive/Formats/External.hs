{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Archive.Formats.External (
    Archive(..),
    Unknown(..),
    Debian(..),
    SevenZip(..),
    LRZip(..),
    RZip(..),
    Tar(..),
    Ace(..),
    Amiga(..),
    Ar(..),
    Arc(..),
    Arj(..),
    Dms(..), 
    LHA(..), 
    Rar(..),
    Zoo(..),
    Zpaq(..),
    Cabinet(..), 
    Kgb(..), 
    Alz(..), 
    Zip(..), 
    Bzip2(..),
    Compress(..),
    Gzip(..),
    Lzip(..),
    Lzma(..),
    Lzop(..),
    Xz(..),
) where

import Archive.Helpers
import Archive.Types

import Data.ByteString (ByteString(..))
import Data.Attoparsec.ByteString
import Text.Read (Read(..))
import System.IO

skp n = count n $ anyWord8
may s = option "" $ has s
has s = string s
lt n = satisfy (\c -> c < n)
inb a b = satisfy (\c -> a <= c && c <= b)
ascii = lt 128

cannotBuild _ _ _ _ = return $ Right "Cannot build this type of archive."
cannotExtract _ _ _ = return $ Right "Cannot extract this type of archive."

newArchiveType "Unknown"
    [| cannotBuild |]
    [| cannotExtract |]
    [| has "" |]
    ["Unknown"]

-- | run with normal standard input and output
run cmd args dir = runCommand dir stdin stdout cmd args

newArchiveType "Debian"
    [| cannotBuild |]
    [|(\ _ archive -> run "dpkg-deb" ["-x", archive, "."] )|]
    [| has "!<arch>\ndebian" |]
    ["deb"]

-- | plain create, options with archive and files at the end
plainB cmd opt _ files arch = run cmd $ opt ++ (arch : files)
-- | plain extract, options with archive at the end
plainX cmd opt _ arch = run cmd $ opt ++ [arch]

newArchiveType "SevenZip"
    [| plainB "7z" ["a", "-r"] |]
    [| plainX "7z" ["x"] |]
    [| string "7z\xbc\xaf\x27\x1c" |]
    ["7z"]

newArchiveType "LRZip"
    [| plainB "lrzip" ["-o"] |]
    [| plainX "lrzip" ["-d", "-q", "-o", "file"] |]
    [| string "LRZI" |]
    ["lrz", "lrzip"]

newArchiveType "RZip"
    [| plainB "rzip" ["-k", "-o"] |]
    [| plainX "rzip" ["-d", "-k", "-o", "file"] |]
    [| string "LRZI" |]
    ["lrz", "lrzip"]

newArchiveType "Tar"
    [| plainB "tar" ["-cf"] |]
    [| plainX "tar" ["-xf"] |]
    [| skp 257 >> has "ustar" >> may "  " >> has "\0" |]
    ["tar"]

-- | dos style create "[cmd] c [archive] [files]"
dosB cmd = plainB cmd ["c"]
-- | dos style extract "[cmd] x [archive]"
dosX cmd = plainX cmd ["x"]

newArchiveType "Ace"
    [| cannotBuild |]
    [| dosX "unace" |]
    [| skp 7 >> has "**ACE**" |]
    ["ace"]

newArchiveType "Amiga"
    [| cannotBuild |]
    [| dosX "unadf" |]
    [| has "DOS" >> lt 6 |]
    ["adf", "amiga"]

newArchiveType "Ar"
    [| plainB "ar" ["rc"] |]
    [| dosX "ar" |]
    [| choice [has "<ar>", has "!<arch>"] |]
    ["a", "ar"]

newArchiveType "Arc"
    [| dosB "arc" |]
    [| dosX "arc" |]
    [| has "\x1a" >> lt 0x14 >> count 12 ascii |]
    ["arc"]

newArchiveType "Arj"
    [| dosB "arj" |]
    [| dosX "arj" |]
    [| has "\x60\xea" |]
    ["arj"]

newArchiveType "Dms" 
    [| cannotBuild |]
    [| dosX "xdms" |]
    [| has "DMS!" |]
    ["dms"]

newArchiveType "LHA" 
    [| cannotBuild |]
    [| dosX "lha" |]
    [| skp 2 >> has "-l" >> inb 0x61 0x7a >> inb 0x20 0x7a >> has "-" |]
    ["lha", "lzh"]

newArchiveType "Rar"
    [| dosB "rar" |]
    [| dosX "unrar" |]
    [| has "Rar!" |]
    ["rar"]

newArchiveType "Zoo"
    [| dosB "zoo" |]
    [| dosX "zoo" |]
    [| skp 20 >> has "\xdc\xa7\xc4\xfd" |]
    ["zoo"]

newArchiveType "Zpaq"
    [| dosB "zpaq" |]
    [| dosX "zpaq" |]
    [| has "zPQ" |]
    ["zpaq"]

-- | bare extraction
bareB cmd = plainB cmd []
bareX cmd = plainX cmd []

newArchiveType "Cabinet" 
    [|(\_ files arch -> run "lcab" $ "-r" : files ++ [arch] )|]
    [| bareX "cabextract" |]
    [| has "MSCF\0\0\0\0" |]
    ["cab", "cabinet"]

newArchiveType "Kgb" 
    [| bareB "kgb" |]
    [| bareX "kgb" |]
    [| has "KGB_arch" |]
    ["alz"]

newArchiveType "Alz" 
    [| cannotBuild |]
    [| bareX "unalz" |]
    [| has "ALZ\x01" |]
    ["alz"]

newArchiveType "Zip" 
    [| plainB "zip" ["-r"] |]
    [| bareX "unzip" |]
    [| has "PK\x03\x04" |]
    ["alz"]

-- | gzip style compress "[cmd] -c [files] > archive"
gzB cmd _ files arch dir = runCommand dir stdin arch cmd $ ["-c"] ++ files
-- | gzip style extract "[cmd] -dc [files] > output"
gzX cmd _ arch dir = runCommand dir stdin (dir ++ "/file") cmd ["-dc", arch]

newArchiveType "Bzip2"
    [| gzB "bzip2" |]
    [| gzX "bzip2" |]
    [| has "BZh" |]
    ["bz2", "bzip2", "bzip"]

newArchiveType "Compress"
    [| gzB "compress" |]
    [| gzX "compress" |]
    [| has "\x1f\x9d" |]
    ["Z", "z", "compress"]

newArchiveType "Gzip"
    [| gzB "gzip" |]
    [| gzX "gzip" |]
    [| has "\x1f\x8b" |]
    ["gz", "gzip"]

newArchiveType "Lzip"
    [| gzB "lzip" |]
    [| gzX "lzip" |]
    [| has "LZIP" |]
    ["lz", "lzip"]

newArchiveType "Lzma"
    [| gzB "lzma" |]
    [| gzX "lzma" |]
    [| has "\x5d\0\0" >> skp 9 >> has "\xff" |]
    ["lzma"]

newArchiveType "Lzop"
    [| gzB "lzop" |]
    [| gzX "lzop" |]
    [| has "\x89LZO\0\x0d\x0a\x1a\x0a" |]
    ["lzo", "lzop"]

newArchiveType "Xz"
    [| gzB "xz" |]
    [| gzX "xz" |]
    [| has "\xfd\x37zXZ\000" |]
    ["xz"]

