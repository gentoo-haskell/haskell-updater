module Main where

import Distribution.Gentoo.GHC
import Distribution.Gentoo.Packages
import Distribution.Gentoo.PkgManager

import System.Exit(ExitCode(ExitSuccess), exitWith)

success     :: String -> IO ExitCode
success msg = do putStrLn msg
                 exitWith ExitSuccess

buildPkgsFrom       :: IO [Package] -> PkgManager ->  IO ExitCode
buildPkgsFrom ps pm = do ps' <- ps
                         if null ps'
                           then success "Nothing to build!"
                           else buildPkgs pm ps' >>= exitWith

ghcUpgrade    :: PkgManager -> IO ExitCode
ghcUpgrade pm = do putStrLn "Looking for packages from old GHC installs..."
                   buildPkgsFrom rebuildPkgs pm

ghcCheck    :: PkgManager -> IO ExitCode
ghcCheck pm = do putStrLn "Looking for packages that need to rebuilt..."
                 buildPkgsFrom brokenPkgs pm

