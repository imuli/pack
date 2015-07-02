{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Archive.Helpers ( runCommand ) where

import System.FilePath
import System.IO
import System.Process hiding (runCommand)
import System.Exit

class Handlable a where
    toReadH :: a -> IO Handle
    toWriteH :: a -> IO Handle

instance Handlable Handle where
    toReadH x = return x
    toWriteH x = return x

instance Handlable FilePath where
    toReadH file = openBinaryFile file ReadMode
    toWriteH file = openBinaryFile file WriteMode

runCommand :: (Handlable a, Handlable b) => FilePath -> a -> b -> String -> [String] -> IO ExitCode
runCommand dir input output cmd args = do
    iH <- toReadH input
    oH <- toWriteH output
    (_,_,_,p) <- createProcess (proc cmd args){ cwd = Just dir
                                              , close_fds = True
                                              , std_in = UseHandle iH
                                              , std_out = UseHandle oH
                                              }
    waitForProcess p

