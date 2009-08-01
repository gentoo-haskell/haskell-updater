{- |
   Module      : Main
   Description : The haskell-updater executable
   Copyright   : (c) Ivan Lazar Miljenovic, Stephan Friedrichs 2009
   License     : GPL-2 or later
   Maintainer  : Ivan.Miljenovic@gmail.com

   The executable module of haskell-updater, which finds Haskell
   packages to rebuild after a dep upgrade or a GHC upgrade.
-}
module Main where

import Distribution.Gentoo.GHC
import Distribution.Gentoo.Packages
import Distribution.Gentoo.PkgManager

import Data.Char(toLower)
import Data.List(find)
import Data.Version(showVersion)
import qualified Paths_haskell_updater as Paths(version)
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import System.Exit(ExitCode(..), exitWith)
import System.IO(hPutStrLn, stderr)
import Control.Monad(liftM, liftM2, when, unless, msum)

-- -----------------------------------------------------------------------------
-- The overall program.

main :: IO ()
main = do flags <- parseArgs
          -- Do this after parseArgs in case of --help, etc.
          ver    <- ghcVersion
          pName  <- getProgName
          pLoc   <- ghcLoc
          libDir <- ghcLibDir
          putStrLn $ "Running " ++ pName ++ " using GHC " ++ ver
          putStrLn $ "with executable in " ++ pLoc
          putStrLn $ "and library directory of " ++ libDir
          uncurry actionOf flags

data Action = Rebuild (IO [Package])

actionOf            :: Action -> PkgManager -> IO a
actionOf (Rebuild iopkgs) = buildPkgsFrom iopkgs

-- -----------------------------------------------------------------------------
-- Utility functions

success     :: String -> IO a
success msg = do putStrLn msg
                 exitWith ExitSuccess

die     :: String -> IO a
die msg = do putErrLn msg
             exitWith (ExitFailure 1)

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

-- -----------------------------------------------------------------------------
-- Finding and rebuilding packages

buildPkgsFrom       :: IO [Package] -> PkgManager ->  IO a
buildPkgsFrom ps pm = do ps' <- ps
                         putStrLn "" -- blank line
                         if null ps'
                           then success "Nothing to build!"
                           else buildPkgs pm ps' >>= exitWith

-- -----------------------------------------------------------------------------
-- Command-line arguments

-- Get and parse args
parseArgs :: IO (Action, PkgManager)
parseArgs = do args <- getArgs
               argParser $ getOpt Permute options args

-- Parse args
argParser                :: ([Flag], [String], [String])
                            -> IO (Action, PkgManager)
argParser (fls, oth, []) = do unless (null oth)
                                $ putErrLn
                                $ unwords $ "Unknown options:" : oth
                              when (hasFlag Help) help
                              when (hasFlag Version) version
                              case pm of
                                Left unknownPkgMgr -> err $ unwords [ "Unknown package manager:"
                                                                    , unknownPkgMgr ]
                                Right pm' -> return (action, pm')
  where
    hasFlag f = f `elem` fls

    upgrade = hasFlag Upgrade
    check = hasFlag Check
    packagesToRebuild
        | upgrade == check = putStrLn bothMsg >> liftM2 (++) brokenPkgs rebuildPkgs
        | upgrade          = rebuildPkgs
        | otherwise        = brokenPkgs

    bothMsg = "\nLooking for packages from both old GHC \
              \installs, and those that need to be rebuilt."

    action = Rebuild packagesToRebuild

    pmSpec = msum $ map getPM fls
    enablePretend = if hasFlag Pretend
                    then setPretend
                    else id

    enableNoDeep = setDeep (not (hasFlag NoDeep))
    pm = fmap enableNoDeep $ fmap enablePretend $
         case pmSpec of
           Nothing -> Right defaultPM
           Just pmSpec' -> case choosePM pmSpec' of
                             Nothing -> Left pmSpec'
                             Just pm' -> Right pm'

    choosePM str = find ((==) str' . name) packageManagers
        where
          str' = map toLower str

argParser (_, _, errs)   = die $ unwords $ "Errors in arguments:" : errs


help :: IO a
help = progInfo >>= success

version :: IO a
version = fmap (++ '-' : showVersion Paths.version) getProgName >>= success

err     :: String -> IO a
err msg = liftM addMsg progInfo >>= die
  where
    addMsg str = msg ++ "\n\n"++ str

progInfo :: IO String
progInfo = do pName <- getProgName
              return $ usageInfo (header pName) options
  where
    header pName = pName ++ " -- Find and rebuild packages broken due to either:\n\
                  \            * GHC upgrade\n\
                  \            * Haskell dependency upgrade\n\
                  \         Default action is to do both.\n\
                  \\n\
                  \Usage: " ++ pName ++ " [Option]\n\
                  \\n\
                  \\n\
                  \Options:"

-- -----------------------------------------------------------------------------
-- Command-line flags

data Flag = Help
          | Version
          | PM String
          | Check
          | Upgrade
          | Pretend
	  | NoDeep
          deriving (Eq, Show)

getPM :: Flag -> Maybe String
getPM (PM pm) = Just pm
getPM _       = Nothing

options :: [OptDescr Flag]
options =
    [ Option ['c']      ["dep-check"]       (NoArg Check)
      "Check dependencies of Haskell packages."
    , Option ['u']      ["upgrade"]         (NoArg Upgrade)
      "Rebuild Haskell packages after a GHC upgrade."
    , Option ['P']      ["package-manager"] (ReqArg PM "PM")
      $ "Use package manager PM, where PM can be one of:\n"
            ++ pmList ++ defPM
    , Option ['p']      ["pretend"]         (NoArg Pretend)
      "Only pretend to build packages."
    , Option []         ["no-deep"]         (NoArg NoDeep)
      "Don't pull deep dependencies (--deep with emerge)."
    , Option ['v']      ["version"]         (NoArg Version)
      "Version information."
    , Option ['h', '?'] ["help"]            (NoArg Help)
      "Print this help message."
    ]
    where
      pmList = unlines . map ((++) "  * " . name) $ packageManagers
      defPM = "The default package manager is: " ++ name defaultPM
