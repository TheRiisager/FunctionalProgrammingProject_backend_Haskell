{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_backend (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\benja\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\benja\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\backend-0.0.0-inplace-backend"
dynlibdir  = "C:\\Users\\benja\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1"
datadir    = "C:\\Users\\benja\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\backend-0.0.0"
libexecdir = "C:\\Users\\benja\\AppData\\Roaming\\cabal\\backend-0.0.0-inplace-backend\\x86_64-windows-ghc-9.0.1\\backend-0.0.0"
sysconfdir = "C:\\Users\\benja\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "backend_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "backend_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "backend_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "backend_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "backend_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "backend_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
