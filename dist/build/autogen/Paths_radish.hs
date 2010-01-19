module Paths_radish (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/aconbere/.cabal/bin"
libdir     = "/Users/aconbere/.cabal/lib/radish-0.1/ghc-6.12.1"
datadir    = "/Users/aconbere/.cabal/share/radish-0.1"
libexecdir = "/Users/aconbere/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "radish_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "radish_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "radish_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "radish_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
