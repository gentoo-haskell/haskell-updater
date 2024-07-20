{- |
   Module      : Distribution.Gentoo.Util
   Description : Utility functions
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-2 or later

   Common utility functions.
 -}

{-# LANGUAGE DeriveTraversable #-}

module Distribution.Gentoo.Util
       ( BSFilePath
       , concatMapM
       , breakAll
       , These(..)
       , these
       ) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import qualified Data.List as L
import Data.ByteString.Char8(ByteString)

-- Alias used to indicate that this ByteString represents a FilePath
type BSFilePath = ByteString

concatMapM   :: (a -> IO [b]) -> [a] -> IO [b]
concatMapM f = fmap concat . traverse f

breakAll   :: (a -> Bool) -> [a] -> [[a]]
breakAll p = L.groupBy (const (not . p))

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

these :: (a -> b -> c) -> (a -> c) -> (b -> c) -> These a b -> c
these f _ _ (These a b) = f a b
these _ g _ (This a) = g a
these _ _ h (That b) = h b
