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
       , ghcLoc
       , ghcLibDir
       , libFronts
       , rebuildPkgs
       , brokenPkgs
       ) where

import Distribution.Gentoo.Util
import Distribution.Gentoo.Packages

-- Cabal imports
import Distribution.Simple.PackageIndex( PackageIndex
                                        , brokenPackages
                                        , reverseDependencyClosure)
import Distribution.Simple.GHC(getInstalledPackages, configure)
import Distribution.Simple.Program( ProgramConfiguration
                                  , defaultProgramConfiguration)
import Distribution.Simple.Compiler(PackageDB(GlobalPackageDB))
import Distribution.Simple.Utils(rawSystemStdout)
import Distribution.Verbosity(silent)
import Distribution.Package(PackageName, packageName)
import Distribution.InstalledPackageInfo( InstalledPackageInfo
                                        , InstalledPackageInfo_
                                        , package)
import Distribution.Text(display)

-- Other imports
import Data.Char(isDigit)
import Data.Either(Either(..), partitionEithers)
import Data.Maybe(Maybe(..), maybe, fromJust)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.ByteString.Char8 as BS
import System.FilePath((</>), takeExtension)
import System.Directory( canonicalizePath
                       , doesDirectoryExist
                       , findExecutable)
import Control.Monad(foldM, liftM, unless)

-- -----------------------------------------------------------------------------

-- Common helper utils, etc.

-- Get only the first line of output
rawSysStdOutLine     :: FilePath -> [String] -> IO String
rawSysStdOutLine app = liftM (head . lines) . rawSystemStdout silent app

-- Get the first line of output from calling GHC with the given
-- arguments.
ghcRawOut      :: [String] -> IO String
ghcRawOut args = ghcLoc >>= flip rawSysStdOutLine args

-- Cheat with using fromJust since we know that GHC must be in $PATH
-- somewhere, probably /usr/bin
ghcLoc :: IO FilePath
ghcLoc = liftM fromJust $ findExecutable "ghc"

ghcVersion :: IO String
ghcVersion = liftM (dropWhile (not . isDigit))
             $ ghcRawOut ["--version"]

ghcLibDir :: IO FilePath
ghcLibDir = canonicalizePath =<< ghcRawOut ["--print-libdir"]

-- Return the Gentoo .conf files found in this GHC libdir
confFiles     :: FilePath -> IO [FilePath]
confFiles dir = do let gDir = dir </> "gentoo"
                   exists <- doesDirectoryExist gDir
                   if exists
                     then do conts <- getDirectoryContents' gDir
                             return $ map (gDir </>)
                               $ filter isConf conts
                     else return []
  where
    isConf file = takeExtension file == ".conf"

pkgListPrint :: String -> [Package] -> IO ()
pkgListPrint desc pkgs
    = if null pkgs
      then putStrLn $ unwords ["No", desc, "packages found!"]
      else do putStrLn $ unwords ["Found the following"
                                 , desc, "packages:"]
              printList printPkg pkgs

printList   :: (a -> String) -> [a] -> IO ()
printList f = mapM_ (putStrLn . (++) "  * " . f)

tryMaybe     :: (a -> Maybe b) -> a -> Either a b
tryMaybe f a = maybe (Left a) Right $ f a

-- -----------------------------------------------------------------------------

-- Finding packages installed with other versions of GHC

rebuildPkgs :: IO [Package]
rebuildPkgs = do putStrLn "\nSearching for packages installed with a \
                          \different version of GHC."
                 thisGhc <- ghcLibDir
                 let thisGhc' = BS.pack thisGhc
                 -- It would be nice to do this, but we can't assume
                 -- some crazy user hasn't deleted one of these dirs
                 -- libFronts' <- filterM doesDirectoryExist libFronts
                 pkgs <- liftM notGHC
                         $ concatMapM (checkLibDir thisGhc') libFronts
                 pkgListPrint "old" pkgs
                 return pkgs

checkLibDir                :: BSFilePath -> FilePath -> IO [Package]
checkLibDir thisGhc libDir = pkgsHaveContent (hasDirMatching wanted)
  where
    wanted dir = isValid dir && (not . isInvalid) dir

    isValid = BS.isPrefixOf (BS.pack $ libDir </> allowedDir)
    allowedDir = "ghc"

    isInvalid dir = any (flip BS.isPrefixOf dir)
                    $ thisGhc : map (BS.pack . (</>) libDir) disAllowedDirs
    -- Use a list here in case we have to add more later
    disAllowedDirs = ["ghc-paths"]

-- The possible places GHC could have installed lib directories
libFronts :: [FilePath]
libFronts = do loc <- ["usr", "opt" </> "ghc"]
               lib <- ["lib", "lib64"]
               return $ "/" </> loc </> lib

-- -----------------------------------------------------------------------------

-- Finding broken packages

brokenPkgs :: IO [Package]
brokenPkgs = do putStrLn "\nSearching Haskell libraries with broken dependencies."
                (pns, cnfs) <- brokenConfs
                unless (null pns)
                           $ unknownPackages pns
                (nI, pkgs) <- liftM partitionEithers $ mapM hasFile' cnfs
                unless (null nI)
                           $ unknownFiles nI
                let pkgs' = notGHC pkgs
                pkgListPrint "broken" pkgs'
                return pkgs
    where
      hasFile' f = do mp <- hasFile f
                      return $ maybe (Left f) Right mp

      unknownPackages ps
          = do putStrLn "\nThe following packages don't seem \
                        \to have been installed by your package manager:"
               printList display ps

      unknownFiles fs
          = do putStrLn "\nThe following files are those corresponding \
                         \to packages installed by your package manager\n\
                         \which can't be matched up to the packages that own them."
               printList id fs

-- .conf files from broken packages of this GHC version
brokenConfs :: IO ([PackageName], [FilePath])
brokenConfs = do brkn <- getBroken
                 -- Check if we actually have to go look up files and
                 -- do IO.
                 if null brkn
                   then return ([], [])
                   else do cnfs <- readConf
                           return $ partitionEithers
                                      $ map (matchConf cnfs) brkn

type ConfMap = Map PackageName FilePath

matchConf :: ConfMap -> PackageName -> Either PackageName FilePath
matchConf = tryMaybe . flip Map.lookup

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
