
module Distribution.Gentoo.Types
  ( RunModifier(..)
  , WithCmd(..)
  , WithUserCmd
  , BuildTarget(..)
  , Flag(..)
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

-- Command-line flags
data Flag = HelpFlag
          | VersionFlag
          | PM String
          | CustomPMFlag String
          | FixInvalid
          | RebuildAll
          | Pretend
          | NoDeep
          | QuietFlag
          | VerboseFlag
          | ListOnlyFlag
          | Cmd String
          deriving (Eq, Ord, Show, Read)
