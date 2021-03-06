{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_solutions_stack (
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

bindir     = "/media/HDD/MasterSoftwareEngineering/SSVT2019-Group2/Wouter/Lab3/.stack-work/install/x86_64-linux/c8dfd41241dacf9e054bd999480d41c56c270b6feca487ab7cd284e21ee64f58/8.6.5/bin"
libdir     = "/media/HDD/MasterSoftwareEngineering/SSVT2019-Group2/Wouter/Lab3/.stack-work/install/x86_64-linux/c8dfd41241dacf9e054bd999480d41c56c270b6feca487ab7cd284e21ee64f58/8.6.5/lib/x86_64-linux-ghc-8.6.5/solutions-stack-0.1.0.0-GenbqTT4QZMIuVapY1OXuR"
dynlibdir  = "/media/HDD/MasterSoftwareEngineering/SSVT2019-Group2/Wouter/Lab3/.stack-work/install/x86_64-linux/c8dfd41241dacf9e054bd999480d41c56c270b6feca487ab7cd284e21ee64f58/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/media/HDD/MasterSoftwareEngineering/SSVT2019-Group2/Wouter/Lab3/.stack-work/install/x86_64-linux/c8dfd41241dacf9e054bd999480d41c56c270b6feca487ab7cd284e21ee64f58/8.6.5/share/x86_64-linux-ghc-8.6.5/solutions-stack-0.1.0.0"
libexecdir = "/media/HDD/MasterSoftwareEngineering/SSVT2019-Group2/Wouter/Lab3/.stack-work/install/x86_64-linux/c8dfd41241dacf9e054bd999480d41c56c270b6feca487ab7cd284e21ee64f58/8.6.5/libexec/x86_64-linux-ghc-8.6.5/solutions-stack-0.1.0.0"
sysconfdir = "/media/HDD/MasterSoftwareEngineering/SSVT2019-Group2/Wouter/Lab3/.stack-work/install/x86_64-linux/c8dfd41241dacf9e054bd999480d41c56c270b6feca487ab7cd284e21ee64f58/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "solutions_stack_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "solutions_stack_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "solutions_stack_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "solutions_stack_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "solutions_stack_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "solutions_stack_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
