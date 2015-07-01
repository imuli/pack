module FileType ( FileType(..)
                , normFiletype
                )
  where

import Data.Char
import Data.List
import Data.Maybe
import Text.Read (Read(..), lift)
import Text.ParserCombinators.ReadP (ReadP, (<++), char, choice, many, munch, sepBy, string)

data FileType = Unknown | GZIP | BZIP2 | LZMA | XZ | TAR | ZIP
        deriving (Enum, Eq)

names :: [(FileType, String)]
names = [ (BZIP2, "bz2")
        , (BZIP2, "bzip2")
        , (GZIP, "gz")
        , (GZIP, "gzip")
        , (LZMA, "lzma")
        , (XZ, "xz")
        , (TAR, "tar")
        , (ZIP, "zip")
        ]

instance Show FileType where
    show x = case listToMaybe $ filter (\(y, _) -> x == y) names of
                  Just (_, name) -> name
                  Nothing -> "Unknown"
    showList x = showString $ intercalate "." $ map show x

readP :: ReadP FileType
readP = (choice $ map valueString names)
        <++ allElse Unknown
  where
    valueString (x, y) = string y >> return x
    allElse y = munch (\_ -> True) >> return y

instance Read FileType where
    readPrec = lift $ readP
    readListPrec = lift $ sepBy readP (char '.')

normFiletype :: String -> String
normFiletype filetype = case map toLower filetype of
    ('a':'p':'p':'l':'i':'c':'a':'t':'i':'o':'n':'/':xs) -> normFiletype xs
    ('x':'-':xs) -> normFiletype xs
    x -> x

