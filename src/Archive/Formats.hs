{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}

module Archive.Formats (
    module Archive.Formats.External,
    module Archive.Formats,
) where

import Archive.Formats.External
import Text.Read
import Text.ParserCombinators.ReadP as RP
import Text.ParserCombinators.ReadPrec as RPrec

import Language.Haskell.TH

data Format = forall a. Archive a => Format a

readP :: ReadPrec Format
readP = RPrec.choice $ [ fmap Format (readPrec :: ReadPrec Xz)
                       , fmap Format (readPrec :: ReadPrec Tar)
                       ]

instance Show Format where
    show (Format a) = show a
instance Read Format where
    readPrec =  readP
    readListPrec = lift $ RP.sepBy (RPrec.readPrec_to_P readP 1) (char '.')

build (Format f) = builder f
extract (Format f) = extracter f

