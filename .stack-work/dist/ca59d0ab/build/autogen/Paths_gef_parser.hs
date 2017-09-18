{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_gef_parser (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\vik\\Dropbox\\Code\\Haskell\\gef-parser\\.stack-work\\install\\495683e6\\bin"
libdir     = "C:\\Users\\vik\\Dropbox\\Code\\Haskell\\gef-parser\\.stack-work\\install\\495683e6\\lib\\x86_64-windows-ghc-8.0.2\\gef-parser-0.1.0.0-CjtLEHQBF2s5JyTbbzgnpU"
dynlibdir  = "C:\\Users\\vik\\Dropbox\\Code\\Haskell\\gef-parser\\.stack-work\\install\\495683e6\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\vik\\Dropbox\\Code\\Haskell\\gef-parser\\.stack-work\\install\\495683e6\\share\\x86_64-windows-ghc-8.0.2\\gef-parser-0.1.0.0"
libexecdir = "C:\\Users\\vik\\Dropbox\\Code\\Haskell\\gef-parser\\.stack-work\\install\\495683e6\\libexec"
sysconfdir = "C:\\Users\\vik\\Dropbox\\Code\\Haskell\\gef-parser\\.stack-work\\install\\495683e6\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gef_parser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gef_parser_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "gef_parser_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "gef_parser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gef_parser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gef_parser_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
