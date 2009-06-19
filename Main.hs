module Main where

import Distribution.Gentoo.GHC
import Distribution.Gentoo.Packages
import Distribution.Gentoo.PkgManager

buildPkgsFrom       :: IO [Package] -> PkgManager ->  IO ()
buildPkgsFrom ps pm = do ps' <- ps
                         if null ps'
                           then putStrLn "Nothing to build!"
                           else buildPkgs pm ps' >> return ()

ghcUpgrade    :: PkgManager -> IO ()
ghcUpgrade pm = do putStrLn "Looking for packages from old GHC installs..."
                   buildPkgsFrom rebuildPkgs pm

ghcCheck    :: PkgManager -> IO ()
ghcCheck pm = do putStrLn "Looking for packages that need to rebuilt..."
                 buildPkgsFrom brokenPkgs pm
