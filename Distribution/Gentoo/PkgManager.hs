{- |
   Module      : Distribution.Gentoo.GHC
   Description : Find GHC-related breakages on Gentoo.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-2 or later
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines helper functions to find broken packages in
   GHC, or else find packages installed with older versions of GHC.
 -}
module Distribution.Gentoo.PkgManager
       ( PkgManager(name)
       , portage
       , pkgcore
       , paludis
       , buildPkgs
       ) where

import Distribution.Gentoo.Packages

import System.Process(system)
import System.Exit(ExitCode)

data PkgManager = PM { name :: Name
                     , cmd  :: Command
                     , opts :: [Option]
                     }

type Name = String
type Command = String
type Option = String

portage :: PkgManager
portage = PM "portage" "emerge" ["--deep", "--oneshot", "--keep-going"]

pkgcore :: PkgManager
pkgcore = PM "pkgcore" "pmerge" ["--deep", "--oneshot", "--ignore-failures"]

paludis :: PkgManager
paludis = PM "paludis" "paludis" ["--install", "--preserve-world"
                                 , "--continue-on-failure if-independent"]

buildPkgs    :: PkgManager -> [Package] -> IO ExitCode
buildPkgs pm = system . buildCmd pm

buildCmd       :: PkgManager -> [Package] -> Command
buildCmd pm ps = unwords $ pmc ++ ps'
  where
    pmc = cmd pm : opts pm
    ps' = map printPkg ps
