{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_haskell_bigraph (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/germanganys/PycharmProjects/Functional_2/.stack-work/install/aarch64-osx/bff4f8a97797b06fc41338752bdad29d85fba640abcfcd5aa23116fa0580d8a2/9.2.5/bin"
libdir     = "/Users/germanganys/PycharmProjects/Functional_2/.stack-work/install/aarch64-osx/bff4f8a97797b06fc41338752bdad29d85fba640abcfcd5aa23116fa0580d8a2/9.2.5/lib/aarch64-osx-ghc-9.2.5/haskell-bigraph-0.1.0.0-9mit6DXtHjU8TZe7apdLDG-haskell-bigraph-exe"
dynlibdir  = "/Users/germanganys/PycharmProjects/Functional_2/.stack-work/install/aarch64-osx/bff4f8a97797b06fc41338752bdad29d85fba640abcfcd5aa23116fa0580d8a2/9.2.5/lib/aarch64-osx-ghc-9.2.5"
datadir    = "/Users/germanganys/PycharmProjects/Functional_2/.stack-work/install/aarch64-osx/bff4f8a97797b06fc41338752bdad29d85fba640abcfcd5aa23116fa0580d8a2/9.2.5/share/aarch64-osx-ghc-9.2.5/haskell-bigraph-0.1.0.0"
libexecdir = "/Users/germanganys/PycharmProjects/Functional_2/.stack-work/install/aarch64-osx/bff4f8a97797b06fc41338752bdad29d85fba640abcfcd5aa23116fa0580d8a2/9.2.5/libexec/aarch64-osx-ghc-9.2.5/haskell-bigraph-0.1.0.0"
sysconfdir = "/Users/germanganys/PycharmProjects/Functional_2/.stack-work/install/aarch64-osx/bff4f8a97797b06fc41338752bdad29d85fba640abcfcd5aa23116fa0580d8a2/9.2.5/etc"

getBinDir     = catchIO (getEnv "haskell_bigraph_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "haskell_bigraph_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "haskell_bigraph_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "haskell_bigraph_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_bigraph_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_bigraph_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
