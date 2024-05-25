
module Distribution.Gentoo.Types
  ( RunModifier(..)
  , WithCmd(..)
  , WithUserCmd
  , BuildTarget(..)
  ) where

import Distribution.Gentoo.PkgManager.Types
import Output

-- Full haskell-updater state
data RunModifier = RM { pkgmgr   :: PkgManager
                      , flags    :: [PMFlag]
                      , withCmd  :: WithCmd
                      , rawPMArgs :: [String]
                      , verbosity :: Verbosity
                      , listOnly :: Bool
                      , showHelp :: Bool
                      , showVer :: Bool
                      , target   :: BuildTarget
                      }
                   deriving (Eq, Ord, Show, Read)

data WithCmd = RunOnly
             | PrintOnly
             | PrintAndRun
               deriving (Eq, Ord, Show, Read)

type WithUserCmd = Either String WithCmd

data BuildTarget = OnlyInvalid
                 | AllInstalled -- Rebuild every haskell package
                   deriving (Eq, Ord, Show, Read)
