{- |
   Module      : Distribution.Gentoo.PkgManager.Types

   Types relating to package managers supported by haskell-updater. Much of
   the module is historical in nature (including the 'PkgManager' type which
   shares the same name as 'Mode.PkgManager' from "Distribution.Gentoo.Types.Mode").
 -}

module Distribution.Gentoo.PkgManager.Types
  ( PkgManager(..)
  , PMFlag(..)
  ) where

-- | Defines the available Gentoo package managers.
data PkgManager = Portage
                | PkgCore
                | Paludis
                | InvalidPM String
                | CustomPM String
                  deriving (Eq, Ord, Show, Read)

-- | Different optional flags to be passed to the PM. This is intended to be
--   a unified interface for command line options that have different names in
--   different package managers.
data PMFlag = PretendBuild -- ^ @--pretend@ for portage, @--no-execute@ for paludis
            | UpdateDeep -- ^ @--deep@ for portage, @--complete@ for paludis
            | UpdateAsNeeded -- ^ Default for portage, @--lazy@ for paludis
              deriving (Eq, Ord, Show, Read)
