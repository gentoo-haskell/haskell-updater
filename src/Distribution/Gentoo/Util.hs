{- |
   Module      : Distribution.Gentoo.Util
   Description : Utility functions
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-2 or later

   Common utility functions.
 -}

{-# LANGUAGE DeriveTraversable #-}

module Distribution.Gentoo.Util
       ( -- * Misc
         BSFilePath
       , concatMapM
       , breakAll
         -- * These
       , These(..)
       , these
         -- * NESet
       , NESet(..)
       , singletonNE
       , insertNE
       , memberNE
       , toListNE
       ) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import qualified Data.List as L
import qualified Data.Set as S
import Data.ByteString.Char8(ByteString)

-- | Alias used to indicate that this ByteString represents a FilePath
type BSFilePath = ByteString

-- | @concatMapM f = fmap concat . traverse f@
concatMapM   :: (a -> IO [b]) -> [a] -> IO [b]
concatMapM f = fmap concat . traverse f

-- | @breakAll p = L.groupBy (const (not . p))@
breakAll   :: (a -> Bool) -> [a] -> [[a]]
breakAll p = L.groupBy (const (not . p))

-- | Taken from the [these](https://hackage.haskell.org/package/these) package,
--   the t'These' type represents values with two non-exclusive possibilities.
--
--   This can be useful to represent combinations of two values, where the
--   combination is defined if either input is.
data These a b
    = These a b
    | This a
    | That b
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Bifunctor These where
    bimap f g (These a b) = These (f a) (g b)
    bimap f _ (This a) = This (f a)
    bimap _ g (That b) = That (g b)

instance Bifoldable These where
    bifoldMap f g (These a b) = f a <> g b
    bifoldMap f _ (This a) = f a
    bifoldMap _ g (That b) = g b

instance Bitraversable These where
    bitraverse f g (These a b) = These <$> f a <*> g b
    bitraverse f _ (This a) = This <$> f a
    bitraverse _ g (That b) = That <$> g b

-- | Case analysis for the t'These' type.
these :: (a -> b -> c) -> (a -> c) -> (b -> c) -> These a b -> c
these f _ _ (These a b) = f a b
these _ g _ (This a) = g a
these _ _ h (That b) = h b

-- | A 'S.Set' which can never be empty.
data NESet a = a :~ S.Set a
    deriving (Show, Eq, Ord, Foldable)

instance Ord a => Semigroup (NESet a) where
    (x1 :~ s1) <> (x2 :~ s2) =
        x1 :~ (if x1 == x2 then id else (S.insert x2)) (s1 <> s2)

-- | Create a 'NESet' from a single value.
singletonNE :: a -> NESet a
singletonNE x = x :~ S.empty

-- | Insert a value into a 'NESet'.
insertNE :: Ord a => a -> NESet a -> NESet a
insertNE x n@(y :~ s)
    | x == y = n
    | otherwise = y :~ S.insert x s

-- | Check if a value exists in a 'NESet'.
memberNE :: Ord a => a -> NESet a -> Bool
memberNE x (y :~ s) = x == y || S.member x s

-- | Convert a 'NESet' to a list. The ordering is defined by 'S.toList' of
--   'S.Set'.
toListNE :: Ord a => NESet a -> [a]
toListNE (x :~ s) = S.toList (S.insert x s)
