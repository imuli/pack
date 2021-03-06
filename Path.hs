{-# LANGUAGE TemplateHaskell #-}

module Path (
  commonPath,
  doesPathExist,
  purgePath,
  renamePath,
  absolutePath,
  freePath,
  ) where

import Data.Maybe
import HFlags
import System.Directory
import System.FilePath
import System.IO

defineFlag "f:force" False "Force overwrite."
freePath :: FilePath -> IO () -> IO ()
freePath path todo = do
    exists <- doesPathExist path
    if not flags_force && exists
        then error $ path ++ ": already exists."
        else todo

commonPath :: [FilePath] -> FilePath
commonPath files =
    joinPath $ foldl1 (prefix []) $ map splitDirectories files
  where
    prefix p [] _ = p
    prefix p _ [] = p
    prefix p (x:xs) (y:ys) = if x == y
                                 then x : prefix p xs ys
                                 else p

data PathType = File | Dir

maybePath :: FilePath -> IO (Maybe PathType)
maybePath path = do
    isfile <- doesFileExist path
    isdir <- doesDirectoryExist path
    case (isfile, isdir) of
        (True, _) -> return $ Just File
        (_, True) -> return $ Just Dir
        _ -> return Nothing

doesPathExist :: FilePath -> IO Bool
doesPathExist path = do
    pathtype <- maybePath path
    case pathtype of
        Just _ -> return True
        Nothing -> return False

purgePath :: FilePath -> IO ()
purgePath path = do
    pathtype <- maybePath path
    case pathtype of
        Just File -> removeFile path
        Just Dir -> removeDirectoryRecursive path
        Nothing -> return ()

renamePath :: FilePath -> FilePath -> IO ()
renamePath path dest = do
    pathtype <- maybePath path
    case pathtype of
        Just File -> renameFile path dest
        Just Dir -> renameDirectory path dest
        _ -> return ()

absolutePath :: FilePath -> IO FilePath
absolutePath = fmap normalise . absolutePath'
  where
    absolutePath' path
      | isRelative path = fmap (</> path) getCurrentDirectory
      | otherwise       = return path
