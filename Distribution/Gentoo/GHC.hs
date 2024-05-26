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
import Distribution.Package(mungedId)
import Distribution.InstalledPackageInfo
    (InstalledPackageInfo(sourceLibName), parseInstalledPackageInfo)
import Distribution.Text(display)
import Distribution.Types.LibraryName (LibraryName(..))

-- Other imports
import Data.Char(isDigit)
import Data.Either(partitionEithers)
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
rawSysStdOutLine app args = do
    out <- rawCommand app args
    case lines out of
        [] -> error $ unwords
            [ "rawSysStdOutLine: Empty output from rawCommand"
            , show app, show args ]
        (s:_) -> pure s

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

ghcConfsPath, gentooConfsPath :: FilePath
ghcConfsPath = "package.conf.d"
gentooConfsPath = "gentoo"

-- Return the Gentoo .conf files found in this GHC libdir
listConfFiles :: FilePath -> IO [FilePath]
listConfFiles subdir = do
    dir <- ghcLibDir
    let gDir = dir </> subdir
    exists <- doesDirectoryExist gDir
    if exists
        then do conts <- listDirectory gDir
                return $ map (gDir </>)
                    $ filter isConf conts
        else return []
  where
    isConf file = takeExtension file == ".conf"

newtype CabalPV = CPV { unCPV :: String } -- serialized 'PackageIdentifier'
    deriving (Ord, Eq, Show)

-- Unique (normal) or multiple (broken) mapping
type ConfMap = Map.Map CabalPV [FilePath]

-- Fold Gentoo .conf files from the current GHC version and
-- create a Map
foldConf :: Verbosity -> [FilePath] -> IO ConfMap
foldConf v = foldM (addConf v) Map.empty

-- cabal package text format
-- "[InstalledPackageInfo {installedPackageId = Insta..."
parse_as_cabal_package :: BS.ByteString -> Maybe InstalledPackageInfo
parse_as_cabal_package cont =
    case parseInstalledPackageInfo cont of
        Left _es -> Nothing
        Right (_ws, ipi) -> Just ipi

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

-- | Add this .conf file to the Map
--
--   NOTE: It is important that the 'CabalPV's that are added to the 'ConfMap'
--   should be in their "munged form", which may be z-encoded
--
--   See:
--     * <https://hackage.haskell.org/package/Cabal-syntax-3.12.0.0/docs/Distribution-Types-MungedPackageName.html>
--     * <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/symbol-names>
addConf :: Verbosity -> ConfMap -> FilePath -> IO ConfMap
addConf v cmp conf = do
    bs <- BS.readFile conf
    case
        ( BS.null bs
        , parse_as_ghc_package bs
        , parse_as_cabal_package bs
        ) of
            -- empty files are created for
            -- phony packages like CABAL_CORE_LIB_GHC_PV
            -- and binary-only packages.
            (True, _      , _       ) -> pure cmp
            -- 'parse_as_ghc_package' is more efficient, so try it first
            (_   , Just dn, _       ) -> do
                vsay v $ unwords [conf, "resolved as ghc package:", show dn]
                pure $ pushConf cmp dn conf
            -- 'parse_as_cabal_package' is more flexible, so use it as a fallback
            (_   , _      , Just ipi) -> do
                let cpv = toCPV ipi
                vsay v $ unwords [conf, "resolved as cabal package:", show (cpv, ipi)]
                pure $ pushConf cmp cpv conf
            _                         -> do
                say v $ unwords
                    [ "failed to parse", show conf, ":", show (BS.take 30 bs)]
                return cmp
  where
    pushConf :: ConfMap -> CabalPV -> FilePath -> ConfMap
    pushConf m k fp = Map.insertWith (++) k [fp] m

    -- 'MungedPackageId's get displayed using the z-encoded format, so are
    -- compatible with ghc-pkg. This is important for resolving broken packages
    -- (reported by ghc-pkg) to @.conf@ files!
    toCPV :: InstalledPackageInfo -> CabalPV
    toCPV = CPV . display . mungedId

checkPkgs :: Verbosity
             -> ([CabalPV], [FilePath])
             -> IO ([Package],[CabalPV],[FilePath])
checkPkgs v (pns, gentoo_cnfs) = do
       files_to_pkgs <- resolveFiles gentoo_cnfs
       let (gentoo_files, pkgs) = unzip $ Set.toList files_to_pkgs
           orphan_gentoo_files = gentoo_cnfs L.\\ gentoo_files
       vsay v $ unwords [ "checkPkgs: searching for gentoo .conf orphans"
                        , show (length orphan_gentoo_files)
                        , "of"
                        , show (length gentoo_cnfs)
                        ]
       return (pkgs, pns, orphan_gentoo_files)

-- -----------------------------------------------------------------------------

-- Finding packages installed with other versions of GHC
oldGhcPkgs :: Verbosity -> IO (Set.Set Package)
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
checkLibDirs :: Verbosity -> BSFilePath -> [BSFilePath] -> IO (Set.Set Package)
checkLibDirs v thisGhc libDirs =
    do vsay v $ "checkLibDir ghc libs: " ++ show (thisGhc, libDirs)
       pkgsHaveContent (hasDirMatching wanted)
  where
    wanted dir = isValid dir && (not . isInvalid) dir

    isValid dir = any (`isGhcLibDir` dir) libDirs

#if MIN_VERSION_bytestring(0,11,1)
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
       cnfs <- listConfFiles gentooConfsPath >>= foldConf v
       vsay v $ "brokenConfs: got " ++ show (Map.size cnfs) ++ " '*.conf' files"
       let (known_broken, orphans) = partitionEithers $ map (matchConf cnfs) all_broken
       return (known_broken, orphan_confs ++ L.concat orphans)
  where
    -- Attempt to match the provided broken package to one of the
    -- installed packages.
    matchConf :: ConfMap -> CabalPV -> Either CabalPV [FilePath]
    matchConf cmp cpv =
        case Map.lookup cpv cmp of
            Just fps -> Right fps
            Nothing -> Left cpv

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
       registered_confs <- listConfFiles ghcConfsPath
       confs_to_pkgs <- resolveFiles registered_confs
       let (conf_files, _conf_pkgs) = unzip $ Set.toList confs_to_pkgs
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
    installed_confs  <- listConfFiles gentooConfsPath >>= foldConf v
    registered_confs <- listConfFiles ghcConfsPath >>= foldConf v
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
    registered_confs <- listConfFiles ghcConfsPath >>= foldConf v
    let registered_twice = Map.filter (\fs -> length fs > 1) registered_confs

    -- Double check that all of the "duplicates" are main libraries, since
    -- a package may also have one or more sub-libraries registered as
    -- well.
    rtMainLibs <- foldM
        (\m k -> Map.alterF onlyMultipleMains k m)
        registered_twice
        (Map.keys registered_twice)

    return $ Map.keys rtMainLibs
  where
    -- Filter out all but entries with multiple main libraries
    onlyMultipleMains :: Maybe [FilePath] -> IO (Maybe [FilePath])
    onlyMultipleMains (Just fps@(_:_)) = do
        fps' <- filterM filterMain fps
        -- Remove entries with one or less elements, since they are no longer
        -- relevant to the getRegisteredTwice function
        pure $ if length fps' <= 1 then Nothing else Just fps'
    onlyMultipleMains _ = pure Nothing

    -- Only keep conf files that correspond to main libraries. This runs
    -- 'parse_as_cabal_package' to get
    filterMain :: FilePath -> IO Bool
    filterMain conf = do
        bs <- BS.readFile conf
        let ipi = parse_as_cabal_package bs
        pure $ case sourceLibName <$> ipi of
            Just LMainLibName -> True
            _ -> False

-- -----------------------------------------------------------------------------

allInstalledPackages :: IO (Set.Set Package)
allInstalledPackages = do libDir <- ghcLibDir
                          let libDir' = BS.pack libDir
                          fmap notGHC $ pkgsHaveContent
                                       $ hasDirMatching (==libDir')
