{- |
   Module      : Distribution.Gentoo.PkgManager
   Description : Using package managers in Gentoo.
   Copyright   : (c) Ivan Lazar Miljenovic, Emil Karlson 2010
   License     : GPL-2 or later

   This module defines ways to use different Gentoo package managers.
 -}

{-# LANGUAGE LambdaCase #-}

module Distribution.Gentoo.PkgManager
       ( definedPMs
       , choosePM
       , stringToCustomPM
       , isValidPM
       , defaultPM
       , defaultPMName
       , nameOfPM
       , toPkgManager
       , buildCmd
       , buildRACmd
       ) where

import Distribution.Gentoo.Packages
import Distribution.Gentoo.PkgManager.Types
import Distribution.Gentoo.Types
import qualified Distribution.Gentoo.Types.HUMode as Mode

import Data.Char(toLower)
import Data.Maybe(mapMaybe, fromMaybe)
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.Set as Set
import System.Environment(getEnvironment)

-- -----------------------------------------------------------------------------

-- | The default package manager.  If the environment variable
--   @PACKAGE_MANAGER@ exists, use that; otherwise default to
--   "portage".  Note that even if that environment variable is
--   defined, if it is unknown then it won't be used.
defaultPM :: IO PkgManager
defaultPM = do eDPM <- lookup "PACKAGE_MANAGER" `fmap` getEnvironment
               let dPM = fromMaybe defaultPMName eDPM
                   mPM = dPM `M.lookup` pmNameMap
               return $ fromMaybe knownDef mPM
  where
    knownDef = pmNameMap M.! defaultPMName

defaultPMName :: String
defaultPMName = "portage"

-- | The names of known package managers.
definedPMs :: [String]
definedPMs = M.keys pmNameMap

isValidPM                    :: PkgManager -> Either String PkgManager
isValidPM (InvalidPM pmname) = Left pmname
isValidPM pm                 = Right pm

pmNameMap :: Map String PkgManager
pmNameMap = M.fromList [ ("portage", Portage)
                       , ("pkgcore", PkgCore)
                       , ("paludis", Paludis)
                       ]

pmNameMap' :: Map PkgManager String
pmNameMap' = M.fromList . map (\(nm,pm) -> (pm,nm)) $ M.toList pmNameMap

nameOfPM                    :: PkgManager -> String
nameOfPM (CustomPM pmname)  = "custom package manager command: " ++ pmname
nameOfPM (InvalidPM pmname) = "invalid package manager: " ++ pmname
nameOfPM pm                 = pmNameMap' M.! pm

-- | Choose the appropriate PM from the textual representation; throws
--   an error if that PM isn't known.
choosePM    :: String -> PkgManager
choosePM pm = fromMaybe (InvalidPM pm) $ pm' `M.lookup` pmNameMap
    where
      pm' = map toLower pm

stringToCustomPM :: String -> PkgManager
stringToCustomPM = CustomPM

pmCommand                :: PkgManager -> String
pmCommand Portage        = "emerge"
pmCommand PkgCore        = "pmerge"
pmCommand Paludis        = "cave"
pmCommand (CustomPM cmd) = cmd
pmCommand (InvalidPM _)  = undefined

defaultPMFlags               :: PkgManager -> [String]
defaultPMFlags Portage       = [ "--oneshot"
                               , "--keep-going"
                               , "--complete-graph"
                               ]
defaultPMFlags PkgCore       = [ "--deep"
                               , "--oneshot"
                               , "--ignore-failures"
                               ]
defaultPMFlags Paludis       = [ "resolve"
                               , "--execute"
                               , "--preserve-world"
                               , "--continue-on-failure", "if-independent"
                               ]
defaultPMFlags CustomPM{}    = []
defaultPMFlags (InvalidPM _) = undefined

-- | Convert from a @HUMode@ 'Mode.PkgManager' to a 'PkgManager' as defined
--   in this module.
toPkgManager :: Mode.PkgManager -> PkgManager
toPkgManager (Mode.Portage _) = Portage
toPkgManager (Mode.PkgCore _) = PkgCore
toPkgManager (Mode.Paludis _) = Paludis
toPkgManager (Mode.CustomPM s _) = CustomPM s

buildCmd
    :: Mode.PkgManager
    -> [PMFlag] -- ^ Basic flags
    -> ExtraRawArgs -- ^ hard-coded extra flags
    -> RawPMArgs -- ^ User-supplied flags
    -> Set.Set Target -- ^ Packages to be rebuilt, and extra targets
    -> (String, [String])
buildCmd mpm fs (ExtraRawArgs rawArgs) userArgs targs =
    (  pmCommand pm
    ,  defaultPMFlags pm
    ++ mapMaybe (flagRep pm) fs
    ++ rawArgs
    ++ userArgs
    ++ excl
    ++ printTargets targs
    )
  where
    excl = case pm of
        Portage -> ["--usepkg=n"]
        _ -> []
    pm = toPkgManager mpm

-- | Alternative version of 'buildCmd' which uses experimental @emerge@
--   invocation (using @--reinstall-atoms@). This is only to be used with the
--   'Portage' package manager.
--
--   The rationale is that by marking broken packages by using
--   @--reinstall-atoms@, portage will pretend that they are not yet
--   installed, thus forcing their reinstallation. @--update@ is
--   used and all installed Haskell packages are targeted so that the entire
--   Haskell environment is examined. This has a side-effect of skipping
--   packages that are masked or otherwise unavailable while still rebuilding
--   needed dependencies that have been broken.
buildRACmd
    :: [PMFlag] -- ^ Basic flags
    -> ExtraRawArgs -- ^ hard-coded extra flags
    -> RawPMArgs -- ^ User-supplied flags
    -> PendingPackages -- ^ Packages to be rebuilt
    -> Set.Set Target -- ^ emerge targets
    -> AllPkgs -- ^ for use with 'usepkgExclude'
    -> (String, [String])
buildRACmd fs (ExtraRawArgs rawArgs) userArgs pending targets allPs =
    (  pmCommand Portage
    ,  defaultPMFlags Portage
    ++ mapMaybe (flagRep Portage) fs
    ++ ["--update"]
    ++ rawArgs
    ++ userArgs
    ++ usepkgExclude allPs
    ++ reinst
    ++ printTargets targets
    )
  where
    reinst =
        let raArgs ps
              | Set.null ps = []
              | otherwise = ["--reinstall-atoms", unwords (printPkg <$> Set.toList ps)]
        in raArgs (getPkgs pending)

-- | Print a set of targets, suitable for passing to a package manager
printTargets :: Set.Set Target -> [String]
printTargets targets = Set.toList $ foldr go Set.empty targets
  where
    go targ set = case targ of
        TargetInvalid (InvalidPkgs p) -> foldr (Set.insert . printPkg) set p
        TargetAll (AllPkgs p) -> foldr (Set.insert . printPkg) set p
        CustomTarget t -> Set.insert t set

-- | Generate strings using portage's @--usepkg-exclude@ flag. This filters out
--   dev-haskell/* packages which can be specified using a wildcard, in order
--   to reduce the length of the emerge command a bit.
usepkgExclude :: PackageSet t => t -> [String]
usepkgExclude pkgs0
    | Set.null pkgs = []
    | otherwise = ["--usepkg-exclude", unwords ("dev-haskell/*" : filteredPkgs)]
  where
    filteredPkgs = mapMaybe
        ( \case
              Package "dev-haskell" _ _ -> Nothing
              p -> Just $ printPkg p
        )
        (Set.toList pkgs)
    pkgs = getPkgs pkgs0

-- -----------------------------------------------------------------------------

flagRep               :: PkgManager -> PMFlag -> Maybe String
flagRep Portage       = portagePMFlag
flagRep PkgCore       = pkgcorePMFlag
flagRep Paludis       = cavePMFlag
flagRep CustomPM{}    = const Nothing -- Can't tell how flags would work.
flagRep (InvalidPM _) = undefined

portagePMFlag                :: PMFlag -> Maybe String
portagePMFlag PretendBuild   = Just "--pretend"
portagePMFlag UpdateDeep     = Just "--deep"
portagePMFlag UpdateAsNeeded = Nothing

pkgcorePMFlag :: PMFlag -> Maybe String
pkgcorePMFlag = portagePMFlag -- The options are the same for the 3
                              -- current flags.

cavePMFlag                :: PMFlag -> Maybe String
cavePMFlag PretendBuild   = Just "--no-execute"
cavePMFlag UpdateDeep     = Just "--complete"
cavePMFlag UpdateAsNeeded = Just "--lazy"
