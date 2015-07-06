{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Archive.Identify ( identify, identifyHandle ) where

import Control.Exception (handle, bracket, IOException(..))
import System.FilePath
import System.IO

import Data.Attoparsec.ByteString
import Data.ByteString (hGetContents, ByteString(..))

import Archive.Types

identify :: FilePath -> IO ArchiveType
identify path = handle (\(e :: IOException) -> return Unknown) $
    bracket (openBinaryFile path ReadMode)
            hClose
            identifyHandle

identifyHandle :: Handle -> IO ArchiveType
identifyHandle handle = do
    content <- Data.ByteString.hGetContents handle
    case parseOnly magic content of
         Left e -> return Unknown
         Right r -> return r

magic = choice [ has "!<arch>\ndebian" >> return DEB
               , has "!<arch>" >> return AR
               , has "7z\xbc\xaf\x27\x1c" >> return A7Z
               , has "<ar>" >> return AR
               , has "ALZ\x01" >> return ALZ
               , has "BZh" >> return BZIP2
               , has "DMS!" >> return DMS
               , has "DOS" >> lt 6 >> return ADF
               , has "KGB_arch" >> return KGB
               , has "LRZI" >> return LRZIP
               , has "LZIP" >> return LZIP
               , has "MSCF\0\0\0\0" >> return CAB
               , has "PK\x03\x04" >> return ZIP
               , has "RZIP" >> return RZIP
               , has "Rar!" >> return RAR
               , has "\x1a" >> lt 0x14 >> count 12 ascii >> return ARC
               , has "\x1f\x8b" >> return GZIP
               , has "\x1f\x9d" >> return COMP
               , has "\x5d\0\0" >> skp 9 >> has "\xff" >> return LZMA
               , has "\x60\xea" >> return ARJ
               , has "\x89LZO\0\x0d\x0a\x1a\x0a" >> return LZOP
               , has "\xfd\x37zXZ\000" >> return XZ
               , has "zPQ" >> return ZPAQ
               , skp 2 >> has "-l" >> inb 0x61 0x7a >> inb 0x20 0x7a >> has "-" >> return LHA
               , skp 7 >> has "**ACE**" >> return ACE
               , skp 20 >> has "\xdc\xa7\xc4\xfd" >> return ZOO
               , skp 257 >> has "ustar" >> maybe "  " >> has "\0" >> return TAR
--               , has "Archive\032" >> return ACORN
--               , has "BB02" >> return BACULA
--               , has "LZX" >> return LZX
--               , has "MPQ\x1a" >> return MPQ
--               , has "MSCE\0\0\0\0" >> return WINCE
--               , has "PAR2" >> return PAR
--               , has "Packed File " >> return NWPF
--               , has "SQSH" >> return SQSH
--               , has "UC2\x1a" >> return UC2
--               , has "XPKF" >> return XPKF
--               , has "\032archive" >> return ACORN
--               , has "\x02\x21\x4c\x18" >> return LZ4
--               , has "\x03\x21\x4c\x18" >> return LZ4
--               , has "\x04\x22\x4d\x18" >> return LZ4
--               , has "\x0e\x0f" >> return MSA
--               , has "\x19\x04\x00\x10" >> return SYMBIAN
--               , has "\x1f\xa1" >> return QUASIJARUS
--               , has "\x34\x12\xaa\x55" >> return VPK
--               , has "\xc3\x54\x43\x44" >> return DACT
--               , has "\xff\x06\0\0sNaPpY" >> return SNAPPY
--               , has "qpress10" >> return QPRESS10
--               , has "xar!" >> return XAR
--               , has "{\0\0\0" >> return DAR
--               , skp 2 >> has "-afx-" >> return AFX
               ]
    where
        skp n = count n $ anyWord8
        maybe s = option "" $ has s
        has s = string s
        lt n = satisfy (\c -> c < n)
        inb a b = satisfy (\c -> a <= c && c <= b)
        ascii = lt 128

