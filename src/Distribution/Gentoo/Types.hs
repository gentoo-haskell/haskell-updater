{- |
   Module      : Distribution.Gentoo.Types

   General types needed for haskell-updater functionality
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Distribution.Gentoo.Types
  ( RunModifier(..)
  , RawPMArgs
  , WithCmd(..)
  , WithUserCmd
  , PendingPackages(..)
  , Target(..)
  , RunHistory
  , LoopType(..)
  , ExtraRawArgs(..)
  , InvalidPkgs(..)
  , AllPkgs(..)
  , PackageSet(..)
  ) where

import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import System.Exit (ExitCode(..))

import Distribution.Gentoo.Packages
import Distribution.Gentoo.PkgManager.Types
import Output

-- | Run-mode haskell-updater state
data RunModifier = RM { flags    :: [PMFlag]
                      , withCmd  :: WithCmd
                      , rawPMArgs :: RawPMArgs
                      , verbosity :: Verbosity
                      }
                   deriving (Eq, Ord, Show)

-- | Arguments to be passed when calling the package manager
type RawPMArgs = [String]

data WithCmd = PrintAndRun
             | PrintOnly
             | RunOnly
               deriving (Eq, Ord, Show, Read, Enum, Bounded)

type WithUserCmd = Either String WithCmd

-- | The set of packages that are currently broken and need to be rebuilt,
--   as reported by @ghc-pkg check@. These may or may not equate to the
--   'Target', depending on which mode @haskell-updater@ is running in.
data PendingPackages
    = InvalidPending InvalidPkgs
    | AllPending AllPkgs
    deriving (Show, Eq, Ord)

-- | The "targets" passed to the package manager, for instance package atoms
--   or set strings.
data Target
    = TargetInvalid InvalidPkgs
    | TargetAll AllPkgs
    | CustomTarget String
    deriving (Show, Eq, Ord)

type RunHistory = Seq.Seq (Set.Set Package, ExitCode)

data LoopType
      -- | Loop until there are no pending packages left. Carries a
      --   'Set' of pending packages that were present during previous runs.
      --   Fails if the current 'PendingPackages' matches any in this set,
      --   as this means one or more packages are failing due to reasons other
      --   than broken dependencies.
      --
      --   This is the default "classic" behavior of @haskell-updater@
    = UntilNoPending

      -- | Loop until there is no change in the current pending packages
      --   compared to the last run. Succeeds or fails based on the 'ExitCode'
      --   of the last run and the current run. This is useful for modes where
      --   it is possible to start out with no 'PendingPackages', but some may
      --   appear later as packages are updated and break their dependencies.
      --
      --   Used by e.g. @--mode=reinstall-atoms@.
    | UntilNoChange

      -- | Run once and do not loop. This is useful for modes where no
      --   valuable information can be gleaned by comparing 'PendingPackages'
      --   of separate runs.
      --
      --   Used by e.g. @--target=all@.
    | NoLoop
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Any hard-coded extra raw arguments to pass to the package manager, which
--   are needed for some @haskell-updater@ modes.
newtype ExtraRawArgs = ExtraRawArgs [String]
    deriving (Show, Eq, Ord)

newtype InvalidPkgs = InvalidPkgs (Set.Set Package)
    deriving (Show, Eq, Ord, Semigroup, Monoid)

newtype AllPkgs = AllPkgs (Set.Set Package)
    deriving (Show, Eq, Ord, Semigroup, Monoid)

class PackageSet t where
    getPkgs :: t -> Set.Set Package

instance PackageSet InvalidPkgs where
    getPkgs (InvalidPkgs ps) = ps

instance PackageSet AllPkgs where
    getPkgs (AllPkgs ps) = ps

instance PackageSet () where
    getPkgs () = Set.empty

instance PackageSet PendingPackages where
    getPkgs (InvalidPending p) = getPkgs p
    getPkgs (AllPending p) = getPkgs p
