module Main (module GHC, main) where

{-
  Try to rebuild exact the same versions as before, or just the same slot?
  If rebuilding only the same slot, how do we get the package name without the tailing version?
-}

import System.Directory
import System.IO
import Control.Monad (filterM,when)
import Data.List
import Data.Maybe

-- introduces dep on package filepath
import System.FilePath

import Distribution.Gentoo.GHC as GHC

import qualified Data.ByteString.Char8 as BS

type Category = String
type Package = String

pkgDBDir :: FilePath
pkgDBDir = "/var/db/pkg"

excludePkgs = [ "dev-lang/ghc", "dev-lang/ghc-bin" ]

-- (</>) a b = a ++ '/' : b

forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM = flip mapM

-- proved to be useful
forConcatM lst f = return . concat =<< mapM f lst

getDirectoryContents' dir = do
  is <- getDirectoryContents dir
  return $ (filter (`notElem` [".", ".."])) is

getCurrentGhcLibPath = GHC.ghcLibDir
getCurrentGhcVersion = GHC.ghcVersion

{-
data PkgInfo = PI {
    piPackageName :: String,
    piCategory :: String,
    piSlot :: String
  }
-}

main :: IO ()
main = do
  putStrLn "haskell updater!"
  ghcVersion <- getCurrentGhcVersion
  ghcLibDir <- getCurrentGhcLibPath

  let knownGhcLibPath | "/usr/lib"     `isPrefixOf` ghcLibDir = True
                      | "/opt/ghc/lib" `isPrefixOf` ghcLibDir = True
                      | otherwise = False
  
  when (not knownGhcLibPath) $ do
    putStrLn ""
    putStrLn "[WARNING] Seems you're not using a Gentoo installation of GHC. Are you sure you want to continue?"
    putStrLn $ "[WARNING] Your GHC's library directory is: "
    putStrLn $ "[WARNING] " ++ ghcLibDir
    putStrLn ""

  putStr "Old GHC installations: "
  allGhcDirs <- getAllGhcDirs
  let oldGhcDirs = delete ghcLibDir allGhcDirs
  if (null oldGhcDirs)
    then putStrLn "none"
    else do
      putStrLn ""
      mapM_ (\p -> putStrLn (" - " ++ p)) oldGhcDirs
  putStrLn ""

  putStrLn "looking for packages from older ghc installations..."
  infos <- getPackageInfos
  let infos' = flip filter infos $ \(cat,pkg,_path) ->
                   -- TODO: this check is no good, things could be appended
                   -- to the names. example dev-lang/ghc-foo
                   any ((cat </> pkg) `isPrefixOf`) excludePkgs
  putStrLn (show (length infos) ++ " packages to consider.")

  is <- forConcatM infos $ \pkginfo@(cat,ver,_) -> do
    files <- hasFileInDirs (map BS.pack oldGhcDirs) pkginfo
    let hasFiles = not . null $ files
    when hasFiles $ do
      putStrLn (" * found: " ++ (cat </> ver))
    return [ pkginfo | hasFiles ]
  putStrLn (show (length is) ++ " packages to rebuild")

hasFileInDirs :: [BS.ByteString] -> (Category, Package, FilePath) -> IO [BS.ByteString]
hasFileInDirs dirs (_,_,path) = do
  files <- readContents (path </> "CONTENTS")
  return
    [ p 
    | p <- files
    , d <- dirs
    , d `BS.isPrefixOf` p
    ]

-- |Read a CONTENTS file and return all files (not directories)
readContents :: FilePath -> IO [BS.ByteString]
readContents path = do
  let f x =
        case x of
          [kind, file, _hash, _size] | kind == BS.pack "obj" -> Just file
          _ -> Nothing
  fmap (catMaybes . map f . map BS.words . BS.lines) $ BS.readFile path

-- |Find all GHC dirs in common Gentoo installation paths.
-- This will find both your current ghc version's library path, as well as
-- all the previous ghc installations where the directory still exists.
getAllGhcDirs :: IO [FilePath]
getAllGhcDirs = do
  let libdirs = [ root </> bits 
                | root <- [ "/usr", "/opt/ghc" ]
                , bits <- [ "lib", "lib64" ]
                ]
  libs <- filterM doesDirectoryExist libdirs
  paths <- forConcatM libs $ \libdir -> do
    cont <- getDirectoryContents' libdir
    let f n | "ghc-" `isPrefixOf` n = True
            | otherwise = False
        v = filter f cont
    forConcatM (map (libdir </>) v) $ \ghchome -> do
      -- check that the ghchome dir has a gentoo directory
      -- this should exclude packages like ghc-paths
      hasGHC <- doesDirectoryExist (ghchome </> "gentoo")
      return [ ghchome | hasGHC ]
  fmap nub $ mapM canonicalizePath paths

getPackageInfos :: IO [(Category, Package, FilePath)]
getPackageInfos = do
  cats <- getDirectoryContents' pkgDBDir
  forConcatM [c|c<-cats,c/="world"] $ \cat -> do
    let cat_path = pkgDBDir </> cat
    pkgs <- getDirectoryContents' cat_path
    forConcatM pkgs $ \pkg -> do
      let full_path = cat_path </> pkg
      -- just check that it's a path describing a package, ie. not a cache
      -- or something else.
      exists <- doesFileExist (full_path </> "CONTENTS")
      if exists
        then return [(cat, pkg, full_path)]
        else return []
