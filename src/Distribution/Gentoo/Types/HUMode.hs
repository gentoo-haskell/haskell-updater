{- |
   Module      : Distribution.Gentoo.Types.HUMode
   Description : Valid modes for haskell-updater

   This module houses a complex ADT which represents valid modes for
   haskell-updater. This is converted from @CmdLineArgs@ (in
   @Distribution.Gentoo.CmdLine.Types@), which represents
   possible options given on the command line.
 -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Distribution.Gentoo.Types.HUMode
    ( HUMode(..)
    , PkgManager(..)
    , RunMode(..)
    , runMode
    , Target(..)
    , getTarget
    , PortageMode(..)
    , PortageBasicTarget(..)
    , ReinstallAtomsTarget(..)
    , RATargets
    , getLoopType
    , getExtraRawArgs
    ) where

import Data.Bifoldable (bifoldMap)
import Data.Monoid (Last(..))

import Distribution.Gentoo.Types hiding (Target)
import Distribution.Gentoo.Util (These(..), NESet(..))
import Distribution.Gentoo.PkgManager.Types (PMFlag(..))

data HUMode
    = HelpMode
    | VersionMode
    | RunMode RunModifier PkgManager
    deriving (Eq, Ord, Show)

data PkgManager
    = Portage PortageMode
    | PkgCore RunMode
    | Paludis RunMode
    | CustomPM String RunMode
    deriving (Eq, Ord, Show)

data RunMode
    = BasicMode Target
    | ListMode Target
    deriving (Eq, Ord, Show)

data Target
    = OnlyInvalid
    | AllInstalled
    deriving (Eq, Ord, Show)

type CustomTarget = String

-- | Encodes valid target combinations for 'ReinstallAtomsMode'
type RATargets = These Target (These ReinstallAtomsTarget (NESet CustomTarget))

-- | Convenience function to turn RATargets into a triple of monoids, which helps
--   in the Semigroup definition
toMonoidTriple
    :: RATargets
    -> (Last Target, Last ReinstallAtomsTarget, Maybe (NESet CustomTarget))
toMonoidTriple = bifoldMap
    (\t -> (pure t, mempty, mempty))
    (bifoldMap
        (\r -> (mempty, pure r, mempty))
        (\s -> (mempty, mempty, pure s))
    )

-- | No Monoid instance since there is intentionally no empty element
instance Semigroup RATargets where
    sel1 <> sel2 = case conv (toMonoidTriple sel1 <> toMonoidTriple sel2) of
        (Just t, Just r, Just s) -> These t (These r s)
        (Just t, Just r, Nothing) -> These t (This r)
        (Just t, Nothing, Just s) -> These t (That s)
        (Just t, Nothing, Nothing) -> This t
        (Nothing, Just r, Just s) -> That (These r s)
        (Nothing, Just r, Nothing) -> That (This r)
        (Nothing, Nothing, Just s) -> That (That s)
        -- If there was an option for no targets, it would go here
        (Nothing, Nothing, Nothing) -> undefined
      where
        conv (Last mt, Last mr, ms) = (mt,mr,ms)

data PortageMode
    = PortageBasicMode (Either PortageBasicTarget Target)
    | PortageListMode Target
    | ReinstallAtomsMode RATargets
    deriving (Eq, Ord, Show)

data PortageBasicTarget = PreservedRebuild
    deriving (Eq, Ord, Show)

data ReinstallAtomsTarget
    = WorldTarget
    | WorldFullTarget
    deriving (Eq, Ord, Show)

runMode :: PkgManager -> Either RunMode PortageMode
runMode (Portage rm) = Right rm
runMode (PkgCore rm) = Left rm
runMode (Paludis rm) = Left rm
runMode (CustomPM _ rm) = Left rm

getTarget :: RunMode -> Target
getTarget (BasicMode t) = t
getTarget (ListMode t) = t

-- | Convert from the @haskell-updater@ ADT to 'LoopMode', which encodes
--   how the looping mechanism of @haskell-updater@ should funciton.
--
--   Takes a 'PkgManager' as 'RunMode' is the only mode where looping makes
--   sense. Returns 'NoLoop' if the @--pretend@ flag was passed.
getLoopType :: RunModifier -> PkgManager -> LoopType
getLoopType rm
      -- Always use NoLoop if --pretend is passed on the command line
    | any (== PretendBuild) (flags rm) = const NoLoop
    | otherwise = \case
    -- @--mode=reinstall-atoms@ should not loop if /only/ @--target=all@ is set
    Portage (ReinstallAtomsMode (This AllInstalled)) -> NoLoop

    -- otherwise, it should always use UntilNoChange
    Portage (ReinstallAtomsMode _) -> UntilNoChange

    -- @--target=preserved-rebuild@ should use UntilNoChange
    Portage (PortageBasicMode (Left PreservedRebuild)) -> UntilNoChange

    -- The rest follow a standard pattern
    Portage (PortageBasicMode (Right t)) -> fromTarget t
    Portage (PortageListMode _) -> NoLoop
    PkgCore mode -> fromRunMode mode
    Paludis mode -> fromRunMode mode
    CustomPM _ mode -> fromRunMode mode
  where
    fromRunMode :: RunMode -> LoopType
    fromRunMode = \case
        BasicMode t -> fromTarget t
        ListMode _ -> NoLoop

    fromTarget :: Target -> LoopType
    fromTarget = \case
        OnlyInvalid -> UntilNoPending
        AllInstalled -> NoLoop

-- | Convert from the @haskell-updater@ ADT to 'ExtraRawArgs', hard-coded extra
--   arguments that will be passed to the package manager.
--
--   Takes a 'PkgManager' as 'RunMode' is the only mode which runs a package
--   manager.
getExtraRawArgs :: PkgManager -> ExtraRawArgs
getExtraRawArgs = ExtraRawArgs . \case
    Portage (ReinstallAtomsMode t) ->
        bifoldMap (const []) (bifoldMap fromRAT (const [])) t
    _ -> []
  where
    fromRAT = \case
        WorldFullTarget -> ["--newuse", "--with-bdeps=y"]
        WorldTarget -> []
