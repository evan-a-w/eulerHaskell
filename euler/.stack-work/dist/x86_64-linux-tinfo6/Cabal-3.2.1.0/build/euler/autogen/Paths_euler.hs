{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_euler (
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

bindir     = "/home/evan/dev/euler_haskell/euler/.stack-work/install/x86_64-linux-tinfo6/4602a94d4fe00886c5b83d0c61e9f614cba07eeb6b20b4ac6e4a7d4d422d29f5/8.10.4/bin"
libdir     = "/home/evan/dev/euler_haskell/euler/.stack-work/install/x86_64-linux-tinfo6/4602a94d4fe00886c5b83d0c61e9f614cba07eeb6b20b4ac6e4a7d4d422d29f5/8.10.4/lib/x86_64-linux-ghc-8.10.4/euler-0.1.0.0-GFfGmD7kmQwCaCjdMCGXgj-euler"
dynlibdir  = "/home/evan/dev/euler_haskell/euler/.stack-work/install/x86_64-linux-tinfo6/4602a94d4fe00886c5b83d0c61e9f614cba07eeb6b20b4ac6e4a7d4d422d29f5/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/evan/dev/euler_haskell/euler/.stack-work/install/x86_64-linux-tinfo6/4602a94d4fe00886c5b83d0c61e9f614cba07eeb6b20b4ac6e4a7d4d422d29f5/8.10.4/share/x86_64-linux-ghc-8.10.4/euler-0.1.0.0"
libexecdir = "/home/evan/dev/euler_haskell/euler/.stack-work/install/x86_64-linux-tinfo6/4602a94d4fe00886c5b83d0c61e9f614cba07eeb6b20b4ac6e4a7d4d422d29f5/8.10.4/libexec/x86_64-linux-ghc-8.10.4/euler-0.1.0.0"
sysconfdir = "/home/evan/dev/euler_haskell/euler/.stack-work/install/x86_64-linux-tinfo6/4602a94d4fe00886c5b83d0c61e9f614cba07eeb6b20b4ac6e4a7d4d422d29f5/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "euler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "euler_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "euler_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "euler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "euler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "euler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
