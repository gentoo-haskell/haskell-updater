{- |
   Module      : Distribution.Gentoo.Util
   Description : Utility functions
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-2 or later

   Common utility functions.
 -}
module Distribution.Gentoo.Util
       ( BSFilePath
       , concatMapM
       , breakAll
       ) where

import qualified Data.List as L
import Data.ByteString.Char8(ByteString)
import Control.Monad(liftM)

-- Alias used to indicate that this ByteString represents a FilePath
type BSFilePath = ByteString

concatMapM   :: (a -> IO [b]) -> [a] -> IO [b]
concatMapM f = liftM concat . mapM f

breakAll   :: (a -> Bool) -> [a] -> [[a]]
breakAll p = L.groupBy (const (not . p))
