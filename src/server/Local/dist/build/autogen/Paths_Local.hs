module Paths_Local (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Users\\Zach\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Zach\\AppData\\Roaming\\cabal\\Local-0.1.0.0\\ghc-7.6.3"
datadir    = "C:\\Users\\Zach\\AppData\\Roaming\\cabal\\Local-0.1.0.0"
libexecdir = "C:\\Users\\Zach\\AppData\\Roaming\\cabal\\Local-0.1.0.0"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Local_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Local_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Local_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Local_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
