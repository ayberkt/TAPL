module Paths_TAPL (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/ayberkt/Developer/TAPL/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/bin"
libdir     = "/Users/ayberkt/Developer/TAPL/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/lib/x86_64-osx-ghc-7.10.2/TAPL-0.1.0.0-8wcoRA5XCyFIAkM9i2Cpbs"
datadir    = "/Users/ayberkt/Developer/TAPL/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/share/x86_64-osx-ghc-7.10.2/TAPL-0.1.0.0"
libexecdir = "/Users/ayberkt/Developer/TAPL/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/libexec"
sysconfdir = "/Users/ayberkt/Developer/TAPL/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TAPL_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TAPL_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "TAPL_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TAPL_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TAPL_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
