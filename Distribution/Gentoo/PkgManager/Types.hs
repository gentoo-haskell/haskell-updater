
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

-- | Different optional flags to be passed to the PM.
data PMFlag = PretendBuild
            | UpdateDeep
            | UpdateAsNeeded
              deriving (Eq, Ord, Show, Read)
