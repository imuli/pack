module Archive.Types where

import Data.Attoparsec.ByteString
import Data.ByteString

class (Read a, Show a) => Archive a where
    builder :: a -> [FilePath] -> FilePath -> FilePath -> IO (Either Int String)
    extracter :: a -> FilePath -> FilePath -> IO (Either Int String)
    ident :: Parser a

