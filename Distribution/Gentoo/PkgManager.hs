{- |
   Module      : Distribution.Gentoo.PkgManager
   Description : Using package managers in Gentoo.
   Copyright   : (c) Ivan Lazar Miljenovic, Emil Karlson 2010
   License     : GPL-2 or later
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines ways to use different Gentoo package managers.
 -}
module Distribution.Gentoo.PkgManager
       ( PkgManager
       , definedPMs
       , choosePM
       , stringToCustomPM
       , isValidPM
       , defaultPM
       , defaultPMName
       , nameOfPM
       , dummy
       , PMFlag(..)
       , buildCmd
       ) where

import Distribution.Gentoo.Packages

import Data.Char(toLower)
import Data.Maybe(mapMaybe, fromMaybe)
import qualified Data.Map as M
import Data.Map(Map)
import System.IO.Error(try)
import System.Environment(getEnv)

-- -----------------------------------------------------------------------------

-- | Defines the available Gentoo package managers.
data PkgManager = Portage
                | PkgCore
                | Paludis
                | InvalidPM String
                | CustomPM String
                  deriving (Eq, Ord, Show, Read)

-- | A dummy package manager used for testing purposes.
dummy :: PkgManager
dummy = CustomPM "echo"

-- | The default package manager.  If the environment variable
--   @PACKAGE_MANAGER@ exists, use that; otherwise default to
--   "portage".  Note that even if that environment variable is
--   defined, if it is unknown then it won't be used.
defaultPM :: IO PkgManager
defaultPM = do eDPM <- try $ getEnv "PACKAGE_MANAGER"
               let dPM = either (const defaultPMName) id eDPM
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
choosePM pm = maybe (InvalidPM pm) id $ pm' `M.lookup` pmNameMap
    where
      pm' = map toLower pm

stringToCustomPM :: String -> PkgManager
stringToCustomPM = CustomPM

pmCommand                :: PkgManager -> String
pmCommand Portage        = "emerge"
pmCommand PkgCore        = "pmerge"
pmCommand Paludis        = "paludis"
pmCommand (CustomPM cmd) = cmd
pmCommand (InvalidPM _)  = undefined

defaultPMFlags                 :: PkgManager -> [String]
defaultPMFlags Portage         = ["--oneshot", "--keep-going"]
defaultPMFlags PkgCore         = ["--deep", "--oneshot", "--ignore-failures"]
defaultPMFlags Paludis         = [ "--install", "--preserve-world"
                                 , "--continue-on-failure if-independent"]
defaultPMFlags CustomPM{}      = []
defaultPMFlags (InvalidPM _)   = undefined

buildCmd          :: PkgManager -> [PMFlag] -> [Package] -> String
buildCmd pm fs ps = unwords $ pmCommand pm : fs' ++ ps'
    where
      fs' = defaultPMFlags pm ++ mapMaybe (flagRep pm) fs
      ps' = map printPkg ps

-- -----------------------------------------------------------------------------

-- | Different optional flags to be passed to the PM.
data PMFlag = PretendBuild
            | UpdateDeep
            | UpdateAsNeeded
              deriving (Eq, Ord, Show, Read)

flagRep               :: PkgManager -> PMFlag -> Maybe String
flagRep Portage       = portagePMFlag
flagRep PkgCore       = pkgcorePMFlag
flagRep Paludis       = paludisPMFlag
flagRep CustomPM{}    = const Nothing -- Can't tell how flags would work.
flagRep (InvalidPM _) = undefined

portagePMFlag                :: PMFlag -> Maybe String
portagePMFlag PretendBuild   = Just "--pretend"
portagePMFlag UpdateDeep     = Just "--deep"
portagePMFlag UpdateAsNeeded = Nothing

pkgcorePMFlag :: PMFlag -> Maybe String
pkgcorePMFlag = portagePMFlag -- The options are the same for the 3
                              -- current flags.

paludisPMFlag                :: PMFlag -> Maybe String
paludisPMFlag PretendBuild   = Just "--pretend"
paludisPMFlag UpdateDeep     = Just "--dl-upgrade always"
paludisPMFlag UpdateAsNeeded = Just "--dl-upgrade as-needed --dl-new-slots as-needed"
