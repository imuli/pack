module FileType ( FileType(..) ) where

import Data.Char
import Data.List
import Data.Maybe
import Text.Read (Read(..), lift)
import Text.ParserCombinators.ReadP hiding (lift)

data FileType = Unknown
              | A7Z
              | ACE
              | ADF
              | ALZ
              | AR
              | ARC
              | ARJ
              | BZIP2
              | CAB
              | COMP
              | CPIO
              | DEB
              | DMS
              | EXT2
              | GZIP
              | HFS
              | HFS_PLUS
              | ISO
              | KGB
              | LHA
              | LRZIP
              | LZIP
              | LZMA
              | LZOP
              | RAR
              | RZIP
              | SHAR
              | SHIELD
              | TAR
              | VMFS
              | XZ
              | ZIP
              | ZOO
              | ZPAQ
        deriving (Enum, Eq)

names :: [(FileType, String)]
names = [ (Unknown, "Unknown")
        , (A7Z, "7z")
        , (A7Z, "7z-compressed")
        , (ACE, "ace")
        , (ADF, "adf")
        , (ALZ, "alz")
        , (AR, "a")
        , (AR, "archive")
        , (ARC, "arc")
        , (ARJ, "arj")
        , (BZIP2, "bz2")
        , (BZIP2, "bzip2")
        , (CAB, "cab")
        , (CAB, "vnd.ms-cab-compressed")
        , (COMP, "Z")
        , (COMP, "z")
        , (COMP, "compress")
        , (CPIO, "cpio")
        , (DEB, "deb")
        , (DEB, "vnd.debian.binary-package")
        , (DMS, "dms")
        , (GZIP, "gz")
        , (GZIP, "gzip")
        , (KGB, "kgb")
        , (LHA, "lzh")
        , (LHA, "lha")
        , (LHA, "lzs")
        , (LHA, "pma")
        , (LRZIP, "lrz")
        , (LRZIP, "lrzip")
        , (LZIP, "lz")
        , (LZIP, "lzip")
        , (LZMA, "lzma")
        , (LZOP, "lzo")
        , (LZOP, "lzop")
        , (RAR, "rar")
        , (RZIP, "rz")
        , (RZIP, "rzip")
        , (TAR, "tar")
        , (XZ, "xz")
        , (ZIP, "zip")
        , (ZOO, "zoo")
        , (ZPAQ, "zpaq")
        ]

instance Show FileType where
    show x = case listToMaybe $ filter (\(y, _) -> x == y) names of
                  Just (_, name) -> name
                  Nothing -> "Unknown"
    showList x = showString $ intercalate "." $ map show x

readP :: ReadP FileType
readP = ( eat (string "application/")
          $ eat (string "x-")
            $ choice $ map valueString names
        ) <++ allElse Unknown
  where
    eat p q = (p >> q) +++ q
    valueString (x, y) = string y >> return x
    allElse y = munch (\_ -> True) >> return y

instance Read FileType where
    readPrec = lift $ readP
    readListPrec = lift $ sepBy readP (char '.')

