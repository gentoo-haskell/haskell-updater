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

import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8(ByteString)
import Control.Monad

type Category = String
type Package = String
type VersionedPkg = String
type Slot = String

type VCatPkg = (Category, VersionedPkg)

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

ebuildFile   :: VersionedPkg -> FilePath
ebuildFile p = p ++ ".ebuild"

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

hasFile    :: FilePath -> IO (Maybe VCatPkg)
hasFile fp = liftM listToMaybe $ pkgsHaveContent p
  where
    fp' = BS.pack fp
    p = hasObjMatching ((==) fp')

pkgsHaveContent   :: ([Content] -> Bool) -> IO [VCatPkg]
pkgsHaveContent p = installedCats' >>= concatMapM (catHasContent p)

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

getSlot          :: VCatPkg -> IO (Maybe Slot)
getSlot cp@(_,p) = do ex <- doesFileExist eFile
                      if ex
                        then parse
                        else return Nothing
  where
    eFile = pkgPath cp </> ebuildFile p

    parse = do lns <- liftM BS.lines $ BS.readFile eFile
               let sLn = find isSlot lns
               return $ fmap stripSlot sLn

    isSlot = BS.isPrefixOf slotStart
    stripSlot = BS.unpack . BS.init . BS.drop slotStartLen
    slotStart = BS.pack "SLOT=\""
    slotStartLen = BS.length slotStart

hasContentMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasContentMatching p = any p . map pathOf

hasDirMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasDirMatching p = hasContentMatching p . filter isDir

hasObjMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasObjMatching p = hasContentMatching p . filter isObj
