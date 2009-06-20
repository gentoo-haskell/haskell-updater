module Main where

import Distribution.Gentoo.GHC
import Distribution.Gentoo.Packages
import Distribution.Gentoo.PkgManager

import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import System.Exit(ExitCode(ExitSuccess), exitWith)
import Control.Monad(liftM2)

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

ghcBoth    :: PkgManager -> IO ExitCode
ghcBoth pm = do putStrLn "Looking for packages from both old GHC \
                          \installs, and those that need to be rebuilt..."
                flip buildPkgsFrom pm $ liftM2 (++) brokenPkgs rebuildPkgs


data Flag = Help
          | Version
          | PM String
          | Check
          | Upgrade
          | Pretend
          deriving (Eq, Show)

progInfo :: IO String
progInfo = do name <- getProgName
              return $ usageInfo (header name) options
  where
    header name = name ++ " -- Find and rebuild packages broken due to either:\n\
                  \            * GHC upgrade\n\
                  \            * Haskell dependency upgrade\n\
                  \\n\
                  \Usage: " ++ name ++ " [Option]\n\
                  \\n\
                  \\n\
                  \Options:"


options :: [OptDescr Flag]
options =
  [ Option ['c']      ["check"]           (NoArg Check)
            "Check dependencies (Default Action)"
  , Option ['u']      ["upgrade"]         (NoArg Upgrade)
            "Rebuild packages after upgrade"
  , Option ['P']      ["package-manager"] (ReqArg PM "PM")
            "Use package manager PM, where PM can be one of:\n\
              \  * portage (default)\n\
              \  * pkgcore\n\
              \  * paludis"
  , Option ['p']      ["pretend"]         (NoArg Pretend)
            "Pretend to build, currently useless"
  , Option ['v']      ["version"]         (NoArg Version)
            "Version"
  , Option ['h', '?'] ["help"]            (NoArg Help)
            "Print this help message"
  ]
