module FileType where

import Data.Char
import Data.List
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

