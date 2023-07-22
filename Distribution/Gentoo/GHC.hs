{-# LANGUAGE CPP #-}
{- |
   Module      : Distribution.Gentoo.GHC
   Description : Find GHC-related breakages on Gentoo.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-2 or later

   This module defines helper functions to find broken packages in
   GHC, or else find packages installed with older versions of GHC.
 -}
module Distribution.Gentoo.GHC
       ( ghcVersion
       , ghcLoc
       , ghcLibDir
       , oldGhcPkgs
       , brokenPkgs
       , allInstalledPackages
       , unCPV
       ) where

import Distribution.Gentoo.Util
import Distribution.Gentoo.Packages

-- Cabal imports
import qualified Distribution.Simple.Utils as DSU
import Distribution.Verbosity(silent)
import Distribution.Package(PackageIdentifier, packageId)
import Distribution.InstalledPackageInfo(InstalledPackageInfo)
import Distribution.Text(display)

-- Other imports
import Data.Char(isDigit)
import Data.Either(partitionEithers)
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import System.FilePath((</>), takeExtension, pathSeparator)
import System.Directory( canonicalizePath
                       , doesDirectoryExist
                       , findExecutable
                       , listDirectory )
import Control.Monad

import Output

-- -----------------------------------------------------------------------------

-- Common helper utils, etc.

-- Get only the first line of output
rawSysStdOutLine     :: FilePath -> [String] -> IO String
rawSysStdOutLine app = fmap (head . lines) . rawCommand app

rawCommand          :: FilePath -> [String] -> IO String
rawCommand cmd args = do (out,_,_) <- DSU.rawSystemStdInOut
                                                        silent  -- verbosity
                                                        cmd     -- program loc
                                                        args    -- args
#if MIN_VERSION_Cabal(1,18,0)
                                                        Nothing -- cabal-1.18+: new working dir
                                                        Nothing -- cabal-1.18+: new environment
#endif /* MIN_VERSION_Cabal(1,18,0) */
                                                        Nothing -- input text and binary mode
#if MIN_VERSION_Cabal(2,1,0)
                                                        DSU.IODataModeBinary
#else
                                                        False   -- is output in binary mode
#endif /* MIN_VERSION_Cabal(2,1,0) */
#if MIN_VERSION_Cabal(3,2,0)
                         return (BL8.unpack out)
#elif MIN_VERSION_Cabal(2,1,0)
                         case out of
                             ~(DSU.IODataBinary bs) -> return (BL8.unpack bs)
#else
                         return out
#endif /* MIN_VERSION_Cabal(2,1,0) */

-- Get the first line of output from calling GHC with the given
-- arguments.
ghcRawOut      :: [String] -> IO String
ghcRawOut args = ghcLoc >>= flip rawSysStdOutLine args

-- Cheat with using fromJust since we know that GHC must be in $PATH
-- somewhere, probably /usr/bin.
ghcLoc :: IO FilePath
ghcLoc = fromJust <$> findExecutable "ghc"

-- The version of GHC installed.
ghcVersion :: IO String
ghcVersion = dropWhile (not . isDigit) <$> ghcRawOut ["--version"]

-- The directory where GHC has all its libraries, etc.
ghcLibDir :: IO FilePath
ghcLibDir = canonicalizePath =<< ghcRawOut ["--print-libdir"]

ghcPkgRawOut      :: [String] -> IO String
ghcPkgRawOut args = ghcPkgLoc >>= flip rawCommand args

-- Cheat with using fromJust since we know that ghc-pkg must be in $PATH
-- somewhere, probably /usr/bin.
ghcPkgLoc :: IO FilePath
ghcPkgLoc = fromJust <$> findExecutable "ghc-pkg"

data ConfSubdir = GHCConfs
                | GentooConfs

subdirToDirname :: ConfSubdir -> FilePath
subdirToDirname subdir =
    case subdir of
        GHCConfs    -> "package.conf.d"
        GentooConfs -> "gentoo"

-- Return the Gentoo .conf files found in this GHC libdir
listConfFiles :: ConfSubdir -> IO [FilePath]
listConfFiles subdir = do
    dir <- ghcLibDir
    let gDir = dir </> subdirToDirname subdir
    exists <- doesDirectoryExist gDir
    if exists
        then do conts <- listDirectory gDir
                return $ map (gDir </>)
                    $ filter isConf conts
        else return []
  where
    isConf file = takeExtension file == ".conf"

tryMaybe     :: (a -> Maybe b) -> a -> Either a b
tryMaybe f a = maybe (Left a) Right $ f a

newtype CabalPV = CPV { unCPV :: String } -- serialized 'PackageIdentifier'
    deriving (Ord, Eq, Show)

-- Unique (normal) or multiple (broken) mapping
type ConfMap = Map.Map CabalPV [FilePath]

pushConf :: ConfMap -> CabalPV -> FilePath -> ConfMap
pushConf m k v = Map.insertWith (++) k [v] m

-- Attempt to match the provided broken package to one of the
-- installed packages.
matchConf :: ConfMap -> CabalPV -> Either CabalPV [FilePath]
matchConf = tryMaybe . flip Map.lookup

-- Fold Gentoo .conf files from the current GHC version and
-- create a Map
foldConf :: Verbosity -> [FilePath] -> IO ConfMap
foldConf v = foldM (addConf v) Map.empty

-- cabal package text format
-- "[InstalledPackageInfo {installedPackageId = Insta..."
parse_as_cabal_package :: String -> Maybe CabalPV
parse_as_cabal_package cont =
    case reads cont of
        []       -> Nothing
        -- ebuilds that have CABAL_CORE_LIB_GHC_PV set
        -- for this version of GHC will have a .conf
        -- file containing just []
        [([],_)] -> Nothing
        rd       -> Just $ CPV $ display $ cfNm rd
  where
    cfNm :: [([InstalledPackageInfo], String)] -> PackageIdentifier
    cfNm = packageId . head . fst . head

-- ghc package text format
-- "name: zlib-conduit\n
--  version: 1.1.0\n
--  id: zlib-condui..."
parse_as_ghc_package :: BS.ByteString -> Maybe CabalPV
parse_as_ghc_package cont =
    case (map BS.words . BS.lines) cont of
        ( [name_key, bn] : [ver_key, bv] : _)
            | name_key == BS.pack "name:" && ver_key == BS.pack "version:"
            -> Just $ CPV $ BS.unpack bn ++ "-" ++ BS.unpack bv
        _   -> Nothing

-- Add this .conf file to the Map
addConf :: Verbosity -> ConfMap -> FilePath -> IO ConfMap
addConf v cmp conf = do
    cont <- BS.readFile conf
    case ( parse_as_ghc_package cont
         , parse_as_cabal_package (BS.unpack cont)
         ) of
        (Just dn, _) -> do vsay v $ unwords [conf, "resolved as ghc package:", show dn]
                           return $ pushConf cmp dn conf
        (_, Just dn) -> do vsay v $ unwords [conf, "resolved as cabal package:", show dn]
                           return $ pushConf cmp dn conf
        -- empty files are created for
        -- phony packages like CABAL_CORE_LIB_GHC_PV
        -- and binary-only packages.
        _ | BS.null cont
                     -> return cmp
        _            -> do say v $ unwords [ "failed to parse"
                                           , show conf
                                           , ":"
                                           , show (BS.take 30 cont)
                                           ]
                           return cmp

checkPkgs :: Verbosity
             -> ([CabalPV], [FilePath])
             -> IO ([Package],[CabalPV],[FilePath])
checkPkgs v (pns, gentoo_cnfs) = do
       files_to_pkgs <- resolveFiles gentoo_cnfs
       let (gentoo_files, pkgs) = unzip files_to_pkgs
           orphan_gentoo_files = gentoo_cnfs L.\\ gentoo_files
       vsay v $ unwords [ "checkPkgs: searching for gentoo .conf orphans"
                        , show (length orphan_gentoo_files)
                        , "of"
                        , show (length gentoo_cnfs)
                        ]
       return (pkgs, pns, orphan_gentoo_files)

-- -----------------------------------------------------------------------------

-- Finding packages installed with other versions of GHC
oldGhcPkgs :: Verbosity -> IO [Package]
oldGhcPkgs v =
    do thisGhc <- ghcLibDir
       vsay v $ "oldGhcPkgs ghc lib: " ++ show thisGhc
       let thisGhc' = BS.pack thisGhc
       -- It would be nice to do this, but we can't assume
       -- some crazy user hasn't deleted one of these dirs
       -- libFronts' <- filterM doesDirectoryExist libFronts
       notGHC <$> checkLibDirs v thisGhc' libFronts

-- Find packages installed by other versions of GHC in this possible
-- library directory.
checkLibDirs :: Verbosity -> BSFilePath -> [BSFilePath] -> IO [Package]
checkLibDirs v thisGhc libDirs =
    do vsay v $ "checkLibDir ghc libs: " ++ show (thisGhc, libDirs)
       pkgsHaveContent (hasDirMatching wanted)
  where
    wanted dir = isValid dir && (not . isInvalid) dir

    isValid dir = any (`isGhcLibDir` dir) libDirs

#if MIN_VERSION_bytestring(1,11,1)
    -- Correct the path for hadrian-based installations
    -- See https://github.com/gentoo-haskell/haskell-updater/issues/20
    thisGhc' = if BS.isSuffixOf (BS.pack "/lib") thisGhc then BS.dropEnd 4 thisGhc else thisGhc

    -- Invalid if it's this GHC
    isInvalid fp = fp == thisGhc' || BS.isPrefixOf (thisGhc' `BS.snoc` pathSeparator) fp
#else
    -- Versions of GHC with old 'bytestring' can use the old behavior
    isInvalid fp = fp == thisGhc || BS.isPrefixOf (thisGhc `BS.snoc` pathSeparator) fp
#endif

-- A valid GHC library directory starting at libdir has a name of
-- "ghc", then a hyphen and then a version number.
isGhcLibDir :: BSFilePath -> BSFilePath -> Bool
isGhcLibDir libdir dir = go ghcDirName
  where
    -- This is hacky because FilePath doesn't work on Bytestrings...
    libdir' = BS.snoc libdir pathSeparator
    ghcDirName = BS.pack "ghc"

    go dn = BS.isPrefixOf ghcDir dir
            -- Any possible version starts with a digit
            && isDigit (BS.index dir ghcDirLen)
      where
        ghcDir = flip BS.snoc '-' $ BS.append libdir' dn
        ghcDirLen = BS.length ghcDir


-- The possible places GHC could have installed lib directories
libFronts :: [BSFilePath]
libFronts = map BS.pack
            $ do lib <- ["lib", "lib64"]
                 return $ "/" </> "usr" </> lib

-- -----------------------------------------------------------------------------

-- Finding broken packages in this install of GHC.
brokenPkgs :: Verbosity -> IO ([Package],[CabalPV],[FilePath])
brokenPkgs v = brokenConfs v >>= checkPkgs v

-- .conf files from broken packages of this GHC version
-- Returns two lists:
-- '[CabalPV]' - list of broken cabal packages we could not resolve
--               to Gentoo's .conf files
-- '[FilePath]' - list of '.conf' files resolved from broken
--                CabalPV reported by 'ghc-pkg check'
brokenConfs :: Verbosity -> IO ([CabalPV], [FilePath])
brokenConfs v =
    do vsay v "brokenConfs: getting broken output from 'ghc-pkg'"
       ghc_pkg_brokens <- getBrokenGhcPkg
       vsay v $ unwords ["brokenConfs: resolving Cabal package names to gentoo equivalents."
                        , show (length ghc_pkg_brokens)
                        , "Cabal packages are broken:"
                        , unwords $ map unCPV ghc_pkg_brokens
                        ]

       (orphan_broken, orphan_confs) <- getOrphanBroken
       vsay v $ unwords [ "brokenConfs: ghc .conf orphans:"
                        , show (length orphan_broken)
                        , "are orphan:"
                        , unwords $ map unCPV orphan_broken
                        ]

       installed_but_not_registered <- getNotRegistered v
       vsay v $ unwords [ "brokenConfs: ghc .conf not registered:"
                        , show (length installed_but_not_registered)
                        , "are not registered:"
                        , unwords $ map unCPV installed_but_not_registered
                        ]

       registered_twice <- getRegisteredTwice v
       vsay v $ unwords [ "brokenConfs: ghc .conf registered twice:"
                        , show (length registered_twice)
                        , "are registered twice:"
                        , unwords $ map unCPV registered_twice
                        ]

       let all_broken = concat [ ghc_pkg_brokens
                               , orphan_broken
                               , installed_but_not_registered
                               , registered_twice
                               ]

       vsay v "brokenConfs: reading '*.conf' files"
       cnfs <- listConfFiles GentooConfs >>= foldConf v
       vsay v $ "brokenConfs: got " ++ show (Map.size cnfs) ++ " '*.conf' files"
       let (known_broken, orphans) = partitionEithers $ map (matchConf cnfs) all_broken
       return (known_broken, orphan_confs ++ L.concat orphans)

-- Return the closure of all packages affected by breakage
-- in format of ["name-version", ... ]
getBrokenGhcPkg :: IO [CabalPV]
getBrokenGhcPkg = map CPV . words
                  <$> ghcPkgRawOut ["check", "--simple-output"]

getOrphanBroken :: IO ([CabalPV], [FilePath])
getOrphanBroken = do
       -- Around Jan 2015 we have started to install
       -- all the .conf files in 'src_install()' phase.
       -- Here we pick orphan ones and notify user about it.
       registered_confs <- listConfFiles GHCConfs
       confs_to_pkgs <- resolveFiles registered_confs
       let (conf_files, _conf_pkgs) = unzip confs_to_pkgs
           orphan_conf_files = registered_confs L.\\ conf_files
       orphan_packages <- fmap catMaybes $
                              forM orphan_conf_files $
                                  fmap parse_as_ghc_package . BS.readFile
       return (orphan_packages, orphan_conf_files)

-- Return packages, that seem to have
-- been installed via emerge (have gentoo/.conf entry),
-- but are not registered in package.conf.d.
-- Usually happens on manual cleaning or
-- due to unregistration bugs in old eclass.
getNotRegistered :: Verbosity -> IO [CabalPV]
getNotRegistered v = do
    installed_confs  <- listConfFiles GentooConfs >>= foldConf v
    registered_confs <- listConfFiles GHCConfs >>= foldConf v
    return $ Map.keys installed_confs L.\\ Map.keys registered_confs

-- Return packages, that seem to have
-- been installed more, than once.
-- It usually happens this way:
--  1. user installs dev-lang/ghc-7.8.4-r0 (comes with bundled transformers-3.0.0.0-ghc-7.8.4-{abi}.conf)
--  2. user installs dev-haskell/transformers-0.4.3.0 (registered as transformers-0.4.3.0-{abi}.conf)
--  3. user upgrades up to dev-lang/ghc-7.8.4-r4 (comes with bundled transformers-0.4.3.0-ghc-7.8.4-{abi}.conf)
-- this way we have single package registered twice:
--   transformers-0.4.3.0-ghc-7.8.4-{abi}.conf
--   transformers-0.4.3.0-{abi}.conf
-- It's is easy to fix just by reinstalling transformers.
getRegisteredTwice :: Verbosity -> IO [CabalPV]
getRegisteredTwice v = do
    registered_confs <- listConfFiles GHCConfs >>= foldConf v
    let registered_twice = Map.filter (\fs -> length fs > 1) registered_confs
    return $ Map.keys registered_twice

-- -----------------------------------------------------------------------------

allInstalledPackages :: IO [Package]
allInstalledPackages = do libDir <- ghcLibDir
                          let libDir' = BS.pack libDir
                          fmap notGHC $ pkgsHaveContent
                                       $ hasDirMatching (==libDir')
