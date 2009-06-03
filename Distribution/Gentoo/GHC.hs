module Distribution.Gentoo.GHC where

import Distribution.Simple.PackageIndex(brokenPackages)
import Distribution.Simple.GHC(getInstalledPackages,configure)
import Distribution.Simple.Program(defaultProgramConfiguration)
import Distribution.Simple.Compiler(PackageDB(GlobalPackageDB))
import Distribution.Verbosity(silent)

import Control.Monad(liftM)

configureGHC = configure silent Nothing Nothing defaultProgramConfiguration

pkgIndex = do (_,conf) <- configureGHC
              getInstalledPackages
                silent
                GlobalPackageDB
                conf

getBroken = liftM brokenPackages pkgIndex
