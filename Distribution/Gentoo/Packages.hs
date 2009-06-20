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
import Data.Maybe(catMaybes, listToMaybe)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8(ByteString)
import System.Directory( doesDirectoryExist
                       , doesFileExist)
import System.FilePath((</>))
import Control.Monad(filterM, liftM)

-- -----------------------------------------------------------------------------

--- Type defns, aliases, etc.

type Category = String
type Pkg = String
type VerPkg = String
type VCatPkg = (Category, VerPkg)
type Slot = String

data Package = Package Category Pkg (Maybe Slot)
             deriving(Eq, Ord, Show, Read)

-- Equality, ignoring the Slot
samePackageAs :: Package -> Package -> Bool
samePackageAs (Package c1 p1 _) (Package c2 p2 _)
  = c1 == c2 && p1 == p2

ghcPkg :: Package
ghcPkg = Package "dev-lang" "ghc" Nothing

ghcBinPkg :: Package
ghcBinPkg = Package "dev-lang" "ghc-bin" Nothing

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

toPackage           :: VCatPkg -> IO Package
toPackage cp@(c,vp) = do sl <- getSlot cp
                         let p = stripVersion vp
                         return $ Package c p sl

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

stripVersion :: VerPkg -> Pkg
stripVersion = concat . takeUntilVer . breakAll partSep
  where
    partSep x = x `elem` "-_"

    -- Only the last bit that matches isVer is the real version bit.
    -- Note that this doesn't check that the last non-version bit is
    -- not a hyphen followed by digits.
    takeUntilVer = concat . init . breakAll isVer

    isVer as = isVerFront (init as) && isAlphaNum (last as)
    isVerFront ('-':as) = all (\a -> isDigit a || a == '.') as
    isVerFront _        = False

pkgPath :: VCatPkg -> FilePath
pkgPath (c,vp) = pkgDBDir </> c </> vp

pkgDBDir :: FilePath
pkgDBDir = "/var/db/pkg"

-- -----------------------------------------------------------------------------

-- Contents

data Content =   Dir BSFilePath
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

-- Searching predicates

hasContentMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasContentMatching p = any p . map pathOf

hasDirMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasDirMatching p = hasContentMatching p . filter isDir

hasObjMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasObjMatching p = hasContentMatching p . filter isObj

-- Parse the CONTENTS file


parseContents    :: VCatPkg -> IO [Content]
parseContents cp = do ex <- doesFileExist cFile
                      if ex
                        then parse
                        else return []
  where
    cFile = pkgPath cp </> "CONTENTS"

    parse = do lns <- liftM BS.lines $ BS.readFile cFile
               return $ catMaybes $ map (parseCLine .BS.words) lns

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
-- Assumes collision protection (i.e. at most one package per file)
hasFile    :: FilePath -> IO (Maybe Package)
hasFile fp = liftM listToMaybe $ pkgsHaveContent p
  where
    fp' = BS.pack fp
    p = hasObjMatching ((==) fp')

pkgsHaveContent   :: ([Content] -> Bool) -> IO [Package]
pkgsHaveContent p = do cs <- installedCats'
                       cps <- concatMapM (catHasContent p) cs
                       mapM toPackage cps

isCat    :: String -> IO Bool
isCat fp = do isD <- doesDirectoryExist (pkgDBDir </> fp)
              return $ isD && isCat' fp
  where
    isCat' ('.':_) = False
    isCat' "world" = False
    isCat' _       = True

installedCats :: IO [Category]
installedCats = filterM isCat =<< getDirectoryContents' pkgDBDir

-- We are most likely to need to look in dev-haskell, so put that
-- first in our list.
installedCats' :: IO [Category]
installedCats' = do cats <- installedCats
                    let cats' = if haskCat `elem` cats
                                then haskCat : delete haskCat cats
                                else cats
                    return cats'
  where
    haskCat = "dev-haskell"

catHasContent     :: ([Content] -> Bool) -> Category -> IO
                     [VCatPkg]
catHasContent p c = do inDir <- getDirectoryContents' cfp
                       let psbl = map ((,) c) inDir
                       pkgs  <- filterM (doesDirectoryExist . pkgPath) psbl
                       filterM (hasContent p) pkgs
  where
    cfp = pkgDBDir </> c

hasContent   :: ([Content] -> Bool) -> VCatPkg -> IO Bool
hasContent p = liftM p . parseContents
