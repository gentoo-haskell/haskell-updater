{- |
   Module      : Distribution.Gentoo.Packages
   Description : Dealing with installed packages on Gentoo.
   Copyright   : (c) Ivan Lazar Miljenovic, Lennart Kolmodin 2009
   License     : GPL-2 or later
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines helper functions that deal with installed
   packages in Gentoo.
-}
module Distribution.Gentoo.Packages
       ( Package
       , Content
       , notGHC
       , printPkg
       , hasFile
       , pkgsHaveContent
       , hasContentMatching
       , hasDirMatching
       , hasObjMatching
       ) where

import Distribution.Gentoo.Util

import Data.Char(isDigit, isAlphaNum)
import Data.List(delete)
import Data.Maybe(mapMaybe, listToMaybe)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8(ByteString)
import System.Directory( doesDirectoryExist
                       , doesFileExist)
import System.FilePath((</>))
import Control.Monad(filterM, liftM)

-- -----------------------------------------------------------------------------

-- Representation of a cat/pkgname in Gentoo.  Note that this is
-- overly simplified.

type Category = String
type Pkg = String -- Package name.
type VerPkg = String -- Package name with version.
type VCatPkg = (Category, VerPkg)
type Slot = String

-- When we are (re-)building packages, we don't care about the
-- version, just the slot.
data Package = Package Category Pkg (Maybe Slot)
             deriving(Eq, Ord, Show, Read)

-- Package equality, ignoring the Slot (i.e. same category and package
-- name).
samePackageAs :: Package -> Package -> Bool
samePackageAs (Package c1 p1 _) (Package c2 p2 _)
  = c1 == c2 && p1 == p2

ghcPkg :: Package
ghcPkg = Package "dev-lang" "ghc" Nothing

ghcBinPkg :: Package
ghcBinPkg = Package "dev-lang" "ghc-bin" Nothing

-- Return all packages that are not a version of GHC.
notGHC :: [Package] -> [Package]
notGHC = filter (\p -> isNot ghcPkg p && isNot ghcBinPkg p)
  where
    isNot p1 = not . samePackageAs p1

-- Pretty-print the Package name based on how PMs expect it
printPkg                 :: Package -> String
printPkg (Package c p s) = addS cp
  where
    addS = maybe id (flip (++) . (:) ':') s
    cp = c ++ '/' : p

-- Determine which slot the specific version of the package is in and
-- create the appropriate Package value.
toPackage           :: VCatPkg -> IO Package
toPackage cp@(c,vp) = do sl <- getSlot cp
                         let p = stripVersion vp
                         return $ Package c p sl

-- Determine which slot the specific version of the package is in.
getSlot    :: VCatPkg -> IO (Maybe Slot)
getSlot cp = do ex <- doesFileExist sFile
                if ex
                  then parse
                  else return Nothing
  where
    sFile = pkgPath cp </> "SLOT"
    parse = do fl <- readFile sFile
               -- Don't want the trailing newline
               return $ listToMaybe $ lines fl

-- | Remove the version information from the package name.
stripVersion :: VerPkg -> Pkg
stripVersion = concat . takeUntilVer . breakAll partSep
  where
    partSep x = x `elem` ['-', '_']

    -- Only the last bit that matches isVer is the real version bit.
    -- Note that this doesn't check that the last non-version bit is
    -- not a hyphen followed by digits.
    takeUntilVer = concat . init . breakAll isVer

    isVer as = isVerFront (init as) && isAlphaNum (last as)
    isVerFront ('-':as) = all (\a -> isDigit a || a == '.') as
    isVerFront _        = False

pkgPath        :: VCatPkg -> FilePath
pkgPath (c,vp) = pkgDBDir </> c </> vp

pkgDBDir :: FilePath
pkgDBDir = "/var/db/pkg"

-- -----------------------------------------------------------------------------

-- Parsing the CONTENTS file of installed packages.

-- Representation of individual lines in a CONTENTS file.
data Content = Dir BSFilePath
             | Obj BSFilePath
               deriving (Eq, Show)

isDir         :: Content -> Bool
isDir (Dir _) = True
isDir _       = False

isObj         :: Content -> Bool
isObj (Obj _) = True
isObj _       = False

pathOf           :: Content -> BSFilePath
pathOf (Dir dir) = dir
pathOf (Obj obj) = obj

-- Searching predicates.

hasContentMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasContentMatching p = any p . map pathOf

hasDirMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasDirMatching p = hasContentMatching p . filter isDir

hasObjMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasObjMatching p = hasContentMatching p . filter isObj

-- Parse the CONTENTS file.
parseContents    :: VCatPkg -> IO [Content]
parseContents cp = do ex <- doesFileExist cFile
                      if ex
                        then parse
                        else return []
  where
    cFile = pkgPath cp </> "CONTENTS"

    parse = do lns <- liftM BS.lines $ BS.readFile cFile
               return $ mapMaybe (parseCLine .BS.words) lns

    -- Use unwords of list rather than taking next element because of
    -- how spaces are represented in file names.
    -- This might cause a problem if there is more than a single
    -- space (or a tab) in the filename...
    parseCLine :: [ByteString] -> Maybe Content
    parseCLine (tp:ln)
      | tp == dir = Just . Dir . BS.unwords $ ln
      | tp == obj = Just . Obj . BS.unwords $ dropLastTwo ln
      | otherwise = Nothing
    parseCLine [] = Nothing

    dropLastTwo :: [a] -> [a]
    dropLastTwo = init . init

    obj = BS.pack "obj"
    dir = BS.pack "dir"

-- -----------------------------------------------------------------------------

-- Find the package (if any) that contain this file.
-- Assumes collision protection (i.e. at most one package per file).
hasFile    :: FilePath -> IO (Maybe Package)
hasFile fp = liftM listToMaybe $ pkgsHaveContent p
  where
    fp' = BS.pack fp
    p = hasObjMatching ((==) fp')

-- Find which packages have Content information that matches the
-- provided predicate; to be used with the searching predicates
-- above.
pkgsHaveContent   :: ([Content] -> Bool) -> IO [Package]
pkgsHaveContent p = do cs <- installedCats'
                       cps <- concatMapM (catHasContent p) cs
                       mapM toPackage cps

-- Determine if this is a valid Category (such that at least one
-- package in that category has been installed).
isCat    :: String -> IO Bool
isCat fp = do isD <- doesDirectoryExist (pkgDBDir </> fp)
              return $ isD && isCat' fp
  where
    isCat' ('.':_) = False
    isCat' "world" = False
    isCat' _       = True

-- Return all Categories known in this system.
installedCats :: IO [Category]
installedCats = filterM isCat =<< getDirectoryContents' pkgDBDir

-- We are most likely to need to look in dev-haskell, so put that
-- first in our list.
installedCats' :: IO [Category]
installedCats' = do cats <- installedCats
                    return $ if haskCat `elem` cats
                             then haskCat : delete haskCat cats
                             else cats
  where
    haskCat = "dev-haskell"

-- Return all packages in this category whose contents match the
-- provided predicate.
catHasContent     :: ([Content] -> Bool) -> Category -> IO [VCatPkg]
catHasContent p c = do inDir <- getDirectoryContents' cfp
                       let psbl = map ((,) c) inDir
                       pkgs  <- filterM (doesDirectoryExist . pkgPath) psbl
                       filterM (hasContent p) pkgs
  where
    cfp = pkgDBDir </> c

-- Determine if this package matches the predicate.
hasContent   :: ([Content] -> Bool) -> VCatPkg -> IO Bool
hasContent p = liftM p . parseContents
