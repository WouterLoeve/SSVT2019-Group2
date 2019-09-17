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

bindir     = "/home/airgetfrog/U_van_A/4-1_SSVT/SSVT2019-Group2/Lab2/.stack-work/install/x86_64-linux/30f2f1eab95b9a7663009e15161688e9d8336047c1e2e128cee39e6140f64ba7/8.6.5/bin"
libdir     = "/home/airgetfrog/U_van_A/4-1_SSVT/SSVT2019-Group2/Lab2/.stack-work/install/x86_64-linux/30f2f1eab95b9a7663009e15161688e9d8336047c1e2e128cee39e6140f64ba7/8.6.5/lib/x86_64-linux-ghc-8.6.5/solutions-stack-0.1.0.0-8Lme4NpTMUv9u3BNgP0hkN"
dynlibdir  = "/home/airgetfrog/U_van_A/4-1_SSVT/SSVT2019-Group2/Lab2/.stack-work/install/x86_64-linux/30f2f1eab95b9a7663009e15161688e9d8336047c1e2e128cee39e6140f64ba7/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/airgetfrog/U_van_A/4-1_SSVT/SSVT2019-Group2/Lab2/.stack-work/install/x86_64-linux/30f2f1eab95b9a7663009e15161688e9d8336047c1e2e128cee39e6140f64ba7/8.6.5/share/x86_64-linux-ghc-8.6.5/solutions-stack-0.1.0.0"
libexecdir = "/home/airgetfrog/U_van_A/4-1_SSVT/SSVT2019-Group2/Lab2/.stack-work/install/x86_64-linux/30f2f1eab95b9a7663009e15161688e9d8336047c1e2e128cee39e6140f64ba7/8.6.5/libexec/x86_64-linux-ghc-8.6.5/solutions-stack-0.1.0.0"
sysconfdir = "/home/airgetfrog/U_van_A/4-1_SSVT/SSVT2019-Group2/Lab2/.stack-work/install/x86_64-linux/30f2f1eab95b9a7663009e15161688e9d8336047c1e2e128cee39e6140f64ba7/8.6.5/etc"

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
