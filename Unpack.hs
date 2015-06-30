{-# LANGUAGE TemplateHaskell #-}

module Main where

import HFlags

defineFlag "k:keep" False "Keep original file."
defineFlag "v:verbose" False "List files as they are unpacked."
return[]

main :: IO ()
main = do
    _ <- $initHFlags "unpack 0.1"
    mapM_ putStrLn arguments

