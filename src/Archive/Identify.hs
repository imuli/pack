{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TemplateHaskell #-}

module Archive.Identify ( identify, identifyHandle ) where

import Control.Exception (handle, bracket, IOException(..))
import System.FilePath
import System.IO

import Data.Attoparsec.ByteString
import Data.ByteString (hGetContents, ByteString(..))

import Archive.Types
import Archive.Formats.External

identify :: (Archive a) => FilePath -> IO a
identify path = handle (\(e :: IOException) -> return Unknown) $
    bracket (openBinaryFile path ReadMode)
            hClose
            identifyHandle

identifyHandle :: (Archive a) => Handle -> IO a
identifyHandle handle = do
    content <- Data.ByteString.hGetContents handle
    case parseOnly magic content of
         Left e -> return Unknown
         Right r -> return r

magic = ident

