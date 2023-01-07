{-# Language TupleSections #-}
{- |
   Module      : Distribution.Gentoo.Packages
   Description : Dealing with installed packages on Gentoo.
   Copyright   : (c) Ivan Lazar Miljenovic, Lennart Kolmodin 2009
   License     : GPL-2 or later

   This module defines helper functions that deal with installed
   packages in Gentoo.
-}
module Distribution.Gentoo.Packages
       ( Package
       , Content
       , notGHC
       , printPkg
       , resolveFiles
       , pkgsHaveContent
       , hasDirMatching
       ) where

import Data.Char(isDigit, isAlphaNum)
import Data.List(isPrefixOf)
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8(ByteString)
import qualified Data.Set as S
import System.Directory( doesDirectoryExist
                       , doesFileExist
                       , listDirectory)
import System.FilePath((</>))
import Control.Monad

import Distribution.Gentoo.Util

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

-- Return all packages that are not a version of GHC.
notGHC :: [Package] -> [Package]
notGHC = filter (isNot ghcPkg)
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
    -- EAPI=5 defines subslots
    split_slot_subslot = break (== '/')
    parse = do fl <- BS.unpack <$> BS.readFile sFile
               -- Don't want the trailing newline
               return $ listToMaybe $ map (fst . split_slot_subslot) $ lines fl

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
pkgPath (c, vp) = pkgDBDir </> c </> vp

pkgDBDir :: FilePath
pkgDBDir = "/var/db/pkg"

-- -----------------------------------------------------------------------------

-- Parsing the CONTENTS file of installed packages.

-- Representation of individual lines in a CONTENTS file.
data Content = Dir BSFilePath
             | Obj BSFilePath
               deriving (Eq, Show, Ord)

isDir         :: Content -> Bool
isDir (Dir _) = True
isDir _       = False

pathOf           :: Content -> BSFilePath
pathOf (Dir dir) = dir
pathOf (Obj obj) = obj

-- Searching predicates.

hasContentMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasContentMatching p = any (p . pathOf)

hasDirMatching   :: (BSFilePath -> Bool) -> [Content] -> Bool
hasDirMatching p = hasContentMatching p . filter isDir

-- Parse the CONTENTS file.
parseContents    :: VCatPkg -> IO [Content]
parseContents cp = do ex <- doesFileExist cFile
                      if ex
                        then parse
                        else return []
  where
    cFile = pkgPath cp </> "CONTENTS"

    parse = do lns <- BS.lines <$> BS.readFile cFile
               return $ mapMaybe (parseCLine . BS.words) lns

    -- Use unwords of list rather than taking next element because of
    -- how spaces are represented in file names.
    -- This might cause a problem if there is more than a single
    -- space (or a tab) in the filename...
    -- Also require at least 3 words in case of an object, as the CONTENTS
    -- file can be corrupt (fixes actual problem reported by user).
    parseCLine :: [ByteString] -> Maybe Content
    parseCLine (tp:ln)
      | tp == dir = Just . Dir . BS.unwords $ ln
      | tp == obj && length ln >= 3 = Just . Obj . BS.unwords $ dropLastTwo ln
      | otherwise = Nothing
    parseCLine [] = Nothing

    dropLastTwo :: [a] -> [a]
    dropLastTwo = init . init

    obj = BS.pack "obj"
    dir = BS.pack "dir"

-- -----------------------------------------------------------------------------

-- Find all the packages that contain given files.
resolveFiles :: [FilePath] -> IO [(FilePath, Package)]
resolveFiles fps = expand <$> forPkg grep
  where
    fps' = S.fromList $ map (Obj . BS.pack) fps
    expand pfs = [ (BS.unpack fn, pn)
                 | (pn, conts) <- pfs
                 , Obj fn <- conts
                 ]
    grep pn cont = Just (pn, filter (\e -> S.member e fps') cont)

-- | Run predecate 'p' for each installed package
--   and gather all 'Just' values
forPkg :: (Package -> [Content] -> Maybe a) -> IO [a]
forPkg p = do
    categories <- installedCats
    catMaybes <$> do
        flip concatMapM categories $ \cat -> do
            maybe_pkgs <- listDirectory (pkgDBDir </> cat)
            packages <- filterM (isPackage . (cat,)) maybe_pkgs
            forM packages $ \pkg -> do
                let cp = (cat, pkg)
                cpn <- toPackage cp
                cont <- parseContents cp
                return $ p cpn cont
  where
    isPackage :: VCatPkg -> IO Bool
    isPackage vcp@(_, vp) = do
        c1 <- doesDirectoryExist $ pkgPath vcp
        let c2 = not $ "-MERGING-" `isPrefixOf` vp
        return $ c1 && c2

-- Find which packages have Content information that matches the
-- provided predicate; to be used with the searching predicates
-- above.
pkgsHaveContent   :: ([Content] -> Bool) -> IO [Package]
pkgsHaveContent p = forPkg p'
    where p' pn cont = if p cont
                           then Just pn
                           else Nothing

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
installedCats = filterM isCat =<< listDirectory pkgDBDir
