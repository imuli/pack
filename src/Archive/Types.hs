module Archive.Types ( Archive(..) ) where

import Data.Attoparsec.ByteString
import Data.ByteString

class (Read a, Show a) => Archive a where
    build :: a -> [FilePath] -> FilePath -> FilePath -> IO (Either Int String)
    extract :: a -> FilePath -> FilePath -> IO (Either Int String)
    ident :: Parser a

