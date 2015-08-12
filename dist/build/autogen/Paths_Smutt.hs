module Paths_Smutt (
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

bindir     = "/Users/tomasmore/Library/Haskell/bin"
libdir     = "/Users/tomasmore/Library/Haskell/ghc-7.8.3-x86_64/lib/Smutt-0.1.0.0"
datadir    = "/Users/tomasmore/Library/Haskell/share/ghc-7.8.3-x86_64/Smutt-0.1.0.0"
libexecdir = "/Users/tomasmore/Library/Haskell/libexec"
sysconfdir = "/Users/tomasmore/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Smutt_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Smutt_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Smutt_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Smutt_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Smutt_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
