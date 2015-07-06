{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TemplateHaskell #-}

module Archive.Identify ( identify, identifyHandle ) where

import Control.Exception (handle, bracket, IOException(..))
import System.FilePath
import System.IO

import Data.Attoparsec.ByteString
import Data.ByteString (hGetContents, ByteString(..))

import Archive.Types
import Archive.Formats

identify :: FilePath -> IO Format
identify path = handle (\(e :: IOException) -> return $ Format Unknown) $
    bracket (openBinaryFile path ReadMode)
            hClose
            identifyHandle

identifyHandle :: Handle -> IO Format
identifyHandle handle = do
    content <- Data.ByteString.hGetContents handle
    case parseOnly magic content of
         Left e -> return $ Format Unknown
         Right r -> return r

magic :: Parser Format
magic = fmap Format $ (ident :: Parser Debian)

