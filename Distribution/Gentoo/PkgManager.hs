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
       ( PkgManager
       , name
       , packageManagers
       , defaultPM
       , dummy
       , setPretend
       , setDeep
       , buildPkgs
       ) where

import Distribution.Gentoo.Packages

import System.Process(system)
import System.Exit(ExitCode)

data PkgManager = PM { name :: Name
                     , cmd  :: Command
                     , opts :: [Option]
		     , deepOpt :: Bool -> [Option] -> [Option]
                     }

type Name = String
type Command = String
type Option = String

packageManagers :: [PkgManager]
packageManagers = [portage, pkgcore, paludis]

defaultPM :: PkgManager
defaultPM = portage

portage :: PkgManager
portage = PM "portage" "emerge" ["--oneshot", "--keep-going"]
				(\deep os -> ["--deep"|deep] ++ os)

pkgcore :: PkgManager
pkgcore = PM "pkgcore" "pmerge" ["--deep", "--oneshot", "--ignore-failures"]
				(\deep os -> ["--deep"|deep] ++ os)

paludis :: PkgManager
paludis = PM "paludis" "paludis" ["--install", "--preserve-world"
                                 , "--continue-on-failure if-independent"
				 ] (\deep os -> ["--dl-upgrade as-needed"|not deep] ++ os)

dummy :: PkgManager
dummy = PM "test PM" "echo" [] (\deep os -> (if deep then "--deep" else "--no-deep") : os)

-- All 3 PMs use --pretend to show what packages they would build;
-- assume that they are all like this.
setPretend    :: PkgManager -> PkgManager
setPretend pm = pm { opts = pOpt : opts pm }
    where
      pOpt = "--pretend"

setDeep :: Bool -> PkgManager -> PkgManager
setDeep isDeep pm = pm { opts = deepOpt pm isDeep (opts pm)}

buildPkgs    :: PkgManager -> [Package] -> IO ExitCode
buildPkgs pm pkgs = do
  putStrLn ("executing: " ++ command)
  system command
  where
    command = buildCmd pm pkgs

buildCmd       :: PkgManager -> [Package] -> Command
buildCmd pm ps = unwords $ pmc ++ ps'
  where
    pmc = cmd pm : opts pm
    ps' = map printPkg ps
