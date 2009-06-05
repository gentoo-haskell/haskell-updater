module Distribution.Gentoo.GHC where

import Distribution.Simple.PackageIndex -- (brokenPackages)
import Distribution.Simple.GHC(getInstalledPackages,configure)
import Distribution.Simple.Program(defaultProgramConfiguration)
import Distribution.Simple.Compiler( PackageDB(GlobalPackageDB)
                                   , compilerVersion)
import Distribution.Simple.Utils(rawSystemStdout)
import Distribution.Verbosity(silent)

import Distribution.Package
import Distribution.InstalledPackageInfo

import Data.Char(isDigit)
import Data.Maybe(fromJust)
import System.FilePath
import System.Directory
import Control.Monad(liftM)

rawSysStdOutLine     :: FilePath -> [String] -> IO String
rawSysStdOutLine app = liftM (head . lines) . rawSystemStdout silent app

ghcRawOut      :: [String] -> IO String
ghcRawOut args = do (Just ghc) <- findExecutable "ghc"
                    rawSysStdOutLine ghc args

ghcVersion :: IO String
ghcVersion = liftM (dropWhile (not . isDigit))
             $ ghcRawOut ["--version"]

ghcLibDir :: IO String
ghcLibDir = canonicalizePath =<< ghcRawOut ["--print-libdir"]

configureGHC = configure silent Nothing Nothing defaultProgramConfiguration

pkgIndex :: IO (PackageIndex InstalledPackageInfo)
pkgIndex = do (_,conf) <- configureGHC
              getInstalledPackages
                silent
                GlobalPackageDB
                conf

getBroken :: IO [InstalledPackageInfo]
getBroken = do ind <- pkgIndex
               let broken = map (package . fst) $ brokenPackages ind
                   brokenClosure = reverseDependencyClosure ind broken
               return brokenClosure

printBroken :: IO ()
printBroken = mapM_ (putStrLn . getName) =<< getBroken

getBroken' = liftM (map getName) getBroken

pkgByName = do ind <- pkgIndex
               let pkgs = allPackages ind
                   pkgs' = map (\p -> (getName p, p)) pkgs
               return pkgs'

pkgName nm = liftM (filter ((==) nm . fst)) pkgByName

getName = drop 6 . (fromJust $ showInstalledPackageInfoField "name")
