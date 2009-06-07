{- |
   Module      : Distribution.Gentoo.GHC
   Description : Find GHC-related breakages on Gentoo.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-2 or later
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines helper functions to find broken packages in
   GHC, or else find packages installed with older versions of GHC.
 -}
module Distribution.Gentoo.GHC where

import Distribution.Simple.PackageIndex -- (brokenPackages)
import Distribution.Simple.GHC(getInstalledPackages,configure)
import Distribution.Simple.Program(defaultProgramConfiguration)
import Distribution.Simple.Compiler( PackageDB(GlobalPackageDB)
                                   , compilerVersion)
import Distribution.Simple.Utils(rawSystemStdout)
import Distribution.Verbosity(silent)

import Distribution.Package
import Distribution.InstalledPackageInfo

import Data.Char(isDigit)
import Data.List(delete,nub,isPrefixOf)
import Data.Maybe(fromJust)
import System.FilePath
import System.Directory
import Control.Monad

-- -----------------------------------------------------------------------------

-- common helper utils, etc.

rawSysStdOutLine     :: FilePath -> [String] -> IO String
rawSysStdOutLine app = liftM (head . lines) . rawSystemStdout silent app

concatMapM   :: (a -> IO [b]) -> [a] -> IO [b]
concatMapM f = liftM concat . mapM f

ghcRawOut      :: [String] -> IO String
ghcRawOut args = do (Just ghc) <- findExecutable "ghc"
                    rawSysStdOutLine ghc args

ghcVersion :: IO String
ghcVersion = liftM (dropWhile (not . isDigit))
             $ ghcRawOut ["--version"]

ghcLibDir :: IO String
ghcLibDir = canonicalizePath =<< ghcRawOut ["--print-libdir"]

confFiles     :: FilePath -> IO [FilePath]
confFiles dir = do let gDir = dir </> "gentoo"
                   exists <- doesDirectoryExist gDir
                   if exists
                     then do conts <- getDirectoryContents gDir
                             return $ map (gDir </>)
                               $ filter isConf conts
                     else return []
  where
    isConf file = (takeExtension file) == ".conf"

-- -----------------------------------------------------------------------------

-- Upgrading

rebuildConfFiles :: IO [FilePath]
rebuildConfFiles = concatMapM confFiles =<< oldGhcLibDirs

libFronts :: [FilePath]
libFronts = do loc <- ["usr", "opt" </> "ghc"]
               lib <- ["lib", "lib64"]
               return $ "/" </> loc </> lib

oldGhcLibDirs :: IO [FilePath]
oldGhcLibDirs = do libDirs <- filterM doesDirectoryExist libFronts
                   -- Remove symlinks, etc.
                   canonLibs <- liftM nub $ mapM canonicalizePath libDirs
                   ghcDirs <- concatMapM getGHCdirs canonLibs
                   thisLib <- ghcLibDir
                   return $ delete thisLib ghcDirs

getGHCdirs     :: FilePath -> IO [FilePath]
getGHCdirs dir = do contents <- getDirectoryContents dir
                    let ghcDs = map (dir </>)
                                -- ghc-paths isn't valid, so remove it
                                . filter (not . isPrefixOf "ghc-paths")
                                $ filter (isPrefixOf "ghc") contents
                    filterM doesDirectoryExist ghcDs

-- -----------------------------------------------------------------------------

-- Fixing

configureGHC = configure silent Nothing Nothing defaultProgramConfiguration

pkgIndex :: IO (PackageIndex InstalledPackageInfo)
pkgIndex = do (_,conf) <- configureGHC
              getInstalledPackages
                silent
                GlobalPackageDB
                conf

getBroken :: IO [InstalledPackageInfo]
getBroken = do ind <- pkgIndex
               let broken = map (package . fst) $ brokenPackages ind
                   brokenClosure = reverseDependencyClosure ind broken
               return brokenClosure

printBroken :: IO ()
printBroken = mapM_ (putStrLn . getName) =<< getBroken

getBroken' = liftM (map getName) getBroken

pkgByName = do ind <- pkgIndex
               let pkgs = allPackages ind
                   pkgs' = map (\p -> (getName p, p)) pkgs
               return pkgs'

pkgName' nm = liftM (filter ((==) nm . fst)) pkgByName

getName = drop 6 . (fromJust $ showInstalledPackageInfoField "name")

{-
getPkgName :: InstalledPackageInfo -> PackageName
getPkgName = packageName
-}
