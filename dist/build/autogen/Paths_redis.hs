module Paths_redis (
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
libdir     = "/Users/aconbere/.cabal/lib/redis-0.1/ghc-6.12.1"
datadir    = "/Users/aconbere/.cabal/share/redis-0.1"
libexecdir = "/Users/aconbere/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "redis_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "redis_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "redis_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "redis_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
