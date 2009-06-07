{- |
   Module      : Distribution.Gentoo.GHC
   Description : Find GHC-related breakages on Gentoo.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-2 or later
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines helper functions to find broken packages in
   GHC, or else find packages installed with older versions of GHC.
 -}
module Distribution.Gentoo.GHC
       ( ghcVersion
       , rebuildConfFiles
       , brokenConfs
       ) where

-- Cabal imports
import Distribution.Simple.PackageIndex( PackageIndex
                                        , brokenPackages
                                        , reverseDependencyClosure)
import Distribution.Simple.GHC(getInstalledPackages, configure)
import Distribution.Simple.Program( ProgramConfiguration
                                  , defaultProgramConfiguration)
import Distribution.Simple.Compiler( PackageDB(GlobalPackageDB)
                                   , compilerVersion)
import Distribution.Simple.Utils(rawSystemStdout)
import Distribution.Verbosity(silent)
import Distribution.Package(PackageName, packageName)
import Distribution.InstalledPackageInfo( InstalledPackageInfo
                                        , InstalledPackageInfo_
                                        , package)

-- Other imports
import Data.Char(isDigit)
import Data.List(delete,nub,isPrefixOf)
import Data.Maybe(fromJust,catMaybes)
import qualified Data.Map as Map
import Data.Map(Map)
import System.FilePath((</>), takeExtension)
import System.Directory( canonicalizePath
                       , doesDirectoryExist
                       , findExecutable
                       , getDirectoryContents)
import Control.Monad(filterM, foldM, liftM)

-- -----------------------------------------------------------------------------

-- common helper utils, etc.

concatMapM   :: (a -> IO [b]) -> [a] -> IO [b]
concatMapM f = liftM concat . mapM f

-- Get only the first line of output
rawSysStdOutLine     :: FilePath -> [String] -> IO String
rawSysStdOutLine app = liftM (head . lines) . rawSystemStdout silent app

-- Get the first line of output from calling GHC with the given
-- arguments.  Cheat with using (Just ...) since we know that GHC
-- must be in $PATH somewhere, probably /usr/bin
ghcRawOut      :: [String] -> IO String
ghcRawOut args = do (Just ghc) <- findExecutable "ghc"
                    rawSysStdOutLine ghc args

ghcVersion :: IO String
ghcVersion = liftM (dropWhile (not . isDigit))
             $ ghcRawOut ["--version"]

ghcLibDir :: IO String
ghcLibDir = canonicalizePath =<< ghcRawOut ["--print-libdir"]

-- Return the Gentoo .conf files found in this GHC libdir
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

-- The list of .conf files from old GHC versions that have to be rebuilt
rebuildConfFiles :: IO [FilePath]
rebuildConfFiles = concatMapM confFiles =<< oldGhcLibDirs

-- The possible places GHC could have installed lib directories
libFronts :: [FilePath]
libFronts = do loc <- ["usr", "opt" </> "ghc"]
               lib <- ["lib", "lib64"]
               return $ "/" </> loc </> lib

-- Lib directories from old GHC versions
oldGhcLibDirs :: IO [FilePath]
oldGhcLibDirs = do libDirs <- filterM doesDirectoryExist libFronts
                   -- Remove symlinks, etc.
                   canonLibs <- liftM nub $ mapM canonicalizePath libDirs
                   ghcDirs <- concatMapM getGHCdirs canonLibs
                   thisLib <- ghcLibDir
                   return $ delete thisLib ghcDirs

-- Find all lib directories from GHC in this possible lib directory
getGHCdirs     :: FilePath -> IO [FilePath]
getGHCdirs dir = do contents <- getDirectoryContents dir
                    let ghcDs = map (dir </>)
                                -- ghc-paths isn't valid, so remove it
                                . filter (not . isPrefixOf "ghc-paths")
                                $ filter (isPrefixOf "ghc") contents
                    filterM doesDirectoryExist ghcDs

-- -----------------------------------------------------------------------------

-- Fixing

-- .conf files from broken packages of this GHC version
brokenConfs :: IO [FilePath]
brokenConfs = do brkn <- getBroken
                 cnfs <- readConf
                 -- Need to think about what to do if PN \notin cnfs
                 return $ catMaybes $ map (flip Map.lookup cnfs) brkn

type ConfMap = Map PackageName FilePath

-- Read in all Gentoo .conf files from the current GHC version and
-- create a Map
readConf :: IO ConfMap
readConf = ghcLibDir >>= confFiles >>= foldM addConf Map.empty

-- Add this .conf file to the Map
addConf          :: ConfMap -> FilePath -> IO ConfMap
addConf cmp conf = do cnts <- readFile conf
                      case (reads cnts) of
                        []       -> return cmp
                        -- ebuilds that have CABAL_CORE_LIB_GHC_PV set
                        -- for this version of GHC will have a .conf
                        -- file containing just []
                        [([],_)] -> return cmp
                        rd       -> do let nm = cfNm rd
                                       return $ Map.insert nm conf cmp

  where
    -- It's not InstalledPackageInfo, as it can't read the modules
    cfNm :: [([InstalledPackageInfo_ String], String)] -> PackageName
    cfNm = packageName . head . fst . head

-- Obtain GHC info about installed libs, etc.
configureGHC :: IO ProgramConfiguration
configureGHC = liftM snd
               $ configure silent Nothing Nothing defaultProgramConfiguration

-- Return all packages registered with GHC
pkgIndex :: IO (PackageIndex InstalledPackageInfo)
pkgIndex = configureGHC >>= getInstalledPackages silent GlobalPackageDB

-- Return the closure of all packages affected by breakage
getBroken :: IO [PackageName]
getBroken = do ind <- pkgIndex
               let broken = map (package . fst) $ brokenPackages ind
               return $ map packageName $ reverseDependencyClosure ind broken
