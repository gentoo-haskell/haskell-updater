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

import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8(ByteString)
import Control.Monad

type Category = String
type Package = String

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

getDirectoryContents'     :: FilePath -> IO [FilePath]
getDirectoryContents' dir = do is <- getDirectoryContents dir
                               return $ filter (`notElem` [".", ".."]) is

isCat    :: String -> IO Bool
isCat fp = do isD <- doesDirectoryExist (pkgDBDir </> fp)
              return $ if isD
                       then isCat' fp
                       else False
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
                    -- technically, we don't know if dev-haskell is
                    -- indeed being used; however, this package will
                    -- be in that category...
                    return $ haskCat : delete haskCat cats
  where
    haskCat = "dev-haskell"

parseContents    :: FilePath -> IO [Content]
parseContents fp = do lns <- liftM BS.lines $ BS.readFile fp
                      return $ catMaybes $ map (parseCLine . BS.words) lns
  where
    parseCLine :: [ByteString] -> Maybe Content
    parseCLine (tp:ln)
      | tp == dir = Just . Dir $ BS.unwords ln
      | tp == obj = Just . Obj . BS.unwords $ dropLastTwo ln
      | otherwise = Nothing
    parseCLine [] = Nothing

    dropLastTwo        :: [a] -> [a]
    dropLastTwo []     = error "Nothing to drop"
    dropLastTwo [_]    = error "Only one thing to drop"
    dropLastTwo [_,_]  = []
    dropLastTwo (a:as) = a : dropLastTwo as

    obj = BS.pack "obj"
    dir = BS.pack "dir"

hasContentMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasContentMatching p = any p . map pathOf

hasDirMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasDirMatching p = hasContentMatching p . filter isDir

hasObjMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasObjMatching p = hasContentMatching p . filter isObj
