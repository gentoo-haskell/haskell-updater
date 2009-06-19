{- |
   Module      : Distribution.Gentoo.Packages
   Description : Dealing with installed packages on Gentoo.
   Copyright   : (c) Ivan Lazar Miljenovic, Lennart Kolmodin 2009
   License     : GPL-2 or later
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines helper functions that deal with installed
   packages in Gentoo.
-}
module Distribution.Gentoo.Packages where

import Distribution.Gentoo.Util

import Data.Char(isDigit)
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8(ByteString)
import Control.Monad

type Category = String
type Pkg = String
type VerPkg = String
type VCatPkg = (Category, VerPkg)
type Slot = String

data Package = Package Category Pkg (Maybe Slot)
             deriving(Eq, Ord, Show, Read)

printPkg                 :: Package -> String
printPkg (Package c p s) = addS cp
  where
    addS = maybe id (flip (++) . (:) ':') s
    cp = c ++ '/':p

type BSFilePath = ByteString

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

pkgDBDir :: FilePath
pkgDBDir = "/var/db/pkg"

pkgPath :: VCatPkg -> FilePath
pkgPath (c,vp) = pkgDBDir </> c </> vp

stripVersion :: VerPkg -> Pkg
stripVersion = concat . takeWhile (not . isVer) . breakAll partSep
  where
    partSep x = x `elem` "-_"

    isVer ('-':as) = all (\a -> isDigit a || a == '.') as
    isVer _        = False

breakAll   :: (a -> Bool) -> [a] -> [[a]]
breakAll p = groupBy (const (not . p))

contents :: FilePath
contents = "CONTENTS"

getDirectoryContents'     :: FilePath -> IO [FilePath]
getDirectoryContents' dir = do is <- getDirectoryContents dir
                               return $ filter (`notElem` [".", ".."]) is

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

hasFile    :: FilePath -> IO (Maybe Package)
hasFile fp = liftM listToMaybe $ pkgsHaveContent p
  where
    fp' = BS.pack fp
    p = hasObjMatching ((==) fp')

pkgsHaveContent   :: ([Content] -> Bool) -> IO [Package]
pkgsHaveContent p = do cs <- installedCats'
                       cps <- concatMapM (catHasContent p) cs
                       mapM toPackage cps

catHasContent     :: ([Content] -> Bool) -> Category -> IO
                     [VCatPkg]
catHasContent p c = do inDir <- getDirectoryContents' cfp
                       let psbl = map ((,) c) inDir
                       pkgs  <- filterM (doesDirectoryExist . pkgPath) psbl
                       filterM (hasContent p) pkgs
  where
    cfp = pkgDBDir </> c
    withDir f x = f $ cfp </> x

hasContent   :: ([Content] -> Bool) -> VCatPkg -> IO Bool
hasContent p = liftM p . parseContents

parseContents    :: VCatPkg -> IO [Content]
parseContents cp = do ex <- doesFileExist cFile
                      if ex
                        then parse
                        else return []
  where
    cFile = pkgPath cp </> contents

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

hasContentMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasContentMatching p = any p . map pathOf

hasDirMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasDirMatching p = hasContentMatching p . filter isDir

hasObjMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasObjMatching p = hasContentMatching p . filter isObj
