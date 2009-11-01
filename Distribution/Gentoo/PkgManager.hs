{- |
   Module      : Distribution.Gentoo.PkgManager
   Description : Using package managers in Gentoo.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-2 or later
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines ways to use different Gentoo package managers.
 -}
module Distribution.Gentoo.PkgManager
       ( PkgManager
       , definedPMs
       , choosePM
       , defaultPM
       , defaultPMName
       , dummy
       , PMFlag(..)
       , buildCmd
       ) where

import Distribution.Gentoo.Packages

import Data.Char(toLower)
import Data.Maybe(mapMaybe, fromMaybe)
import qualified Data.Map as M
import Data.Map(Map)

-- -----------------------------------------------------------------------------

-- | Defines the available Gentoo package managers.
data PkgManager = Portage
                | PkgCore
                | Paludis
                | CustomPM String
                  deriving (Eq, Ord, Show, Read)

-- | A dummy package manager used for testing purposes.
dummy :: PkgManager
dummy = CustomPM "echo"

-- | The default package manager.
defaultPM :: PkgManager
defaultPM = pmNameMap M.! defaultPMName

defaultPMName :: String
defaultPMName = "portage"

-- | The names of known package managers.
definedPMs :: [String]
definedPMs = M.keys pmNameMap

pmNameMap :: Map String PkgManager
pmNameMap = M.fromList [ ("portage", Portage)
                       , ("pkgcore", PkgCore)
                       , ("paludis", Paludis)
                       ]

-- | Choose the appropriate PM from the textual representation; throws
--   an error if that PM isn't known.
choosePM    :: String -> PkgManager
choosePM pm = fromMaybe failure $ pm' `M.lookup` pmNameMap
    where
      failure = error $ "Unknown Package Manager: " ++ pm
      pm' = map toLower pm

pmCommand                :: PkgManager -> String
pmCommand Portage        = "emerge"
pmCommand PkgCore        = "pmerge"
pmCommand Paludis        = "paludis"
pmCommand (CustomPM cmd) = cmd

defaultPMFlags            :: PkgManager -> [String]
defaultPMFlags Portage    = ["--oneshot", "--keep-going"]
defaultPMFlags PkgCore    = ["--deep", "--oneshot", "--ignore-failures"]
defaultPMFlags Paludis    = [ "--install", "--preserve-world"
                            , "--continue-on-failure if-independent"]
defaultPMFlags CustomPM{} = []

buildCmd          :: PkgManager -> [PMFlag] -> [Package] -> String
buildCmd pm fs ps = unwords $ pmCommand pm : fs' ++ ps'
    where
      fs' = mapMaybe (flagRep pm) fs ++ defaultPMFlags pm
      ps' = map printPkg ps

-- -----------------------------------------------------------------------------

-- | Different optional flags to be passed to the PM.
data PMFlag = PretendBuild
            | UpdateDeep
            | UpdateAsNeeded
              deriving (Eq, Ord, Show, Read)

flagRep            :: PkgManager -> PMFlag -> Maybe String
flagRep Portage    = portagePMFlag
flagRep PkgCore    = pkgcorePMFlag
flagRep Paludis    = paludisPMFlag
flagRep CustomPM{} = const Nothing -- Can't tell how flags would work.

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
paludisPMFlag UpdateAsNeeded = Just "--dl-upgrade as-needed"
