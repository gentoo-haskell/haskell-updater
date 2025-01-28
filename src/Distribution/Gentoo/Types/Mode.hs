{- |
   Module      : Distribution.Gentoo.Types.Mode
   Description : Valid modes for haskell-updater

   This module houses a complex ADT which represents valid modes for
   haskell-updater. This is converted from @CmdLineArgs@ (in
   @Distribution.Gentoo.CmdLine.Types@), which represents
   possible options given on the command line.
 -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Distribution.Gentoo.Types.Mode
    ( HUMode(..)
    , PkgManager(..)
    , RunMode(..)
    , runMode
    , Target(..)
    , getTarget
    , PortageMode(..)
    , PortageBasicTarget(..)
    , ReinstallAtomsTarget(..)
    , RATargets(..)
    , getLoopType
    , getExtraRawArgs
    ) where

import Data.Bifoldable (bifoldMap)
import Data.Monoid (Last(..))

import Distribution.Gentoo.Types hiding (Target)
import Distribution.Gentoo.Util (These(..), NESet(..))
import Distribution.Gentoo.PkgManager.Types (PMFlag(..))

-- | Top level run modes for @haskell-updater@ including @--help@ and
--   @--version@, which do not run the @runUpdater@ function in "Main".
--   v'RunMode' is used during normal use of the utility.
data HUMode
    = HelpMode
    | VersionMode
    | RunMode RunModifier PkgManager
    deriving (Eq, Ord, Show)

-- | Choice of supported package managers. Currently, portage has more modes
--   available, such as @--mode=reinstall-atoms@, which are encoded in
--   'PortageMode'.
data PkgManager
    = Portage PortageMode
    | PkgCore RunMode
    | Paludis RunMode
    | CustomPM String RunMode
    deriving (Eq, Ord, Show)

-- | Basic run modes that are available for all package managers.
data RunMode
    = BasicMode Target
    | ListMode Target
    deriving (Eq, Ord, Show)

-- | Basic targets that are available for all package managers.
data Target
    = OnlyInvalid
    | AllInstalled
    deriving (Eq, Ord, Show)

-- | Custom target to be passed to the package manager.
type CustomTarget = String

-- | Encodes valid target combinations for 'ReinstallAtomsMode' (portage only).
newtype RATargets = RATargets
    { unRATargets :: These Target
        (These ReinstallAtomsTarget (NESet CustomTarget)) }
    deriving (Show, Eq, Ord)

-- | No Monoid instance since there is intentionally no empty element
instance Semigroup RATargets where
    sel1 <> sel2 = RATargets $ case conv (toMonoidTriple sel1 <> toMonoidTriple sel2) of
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

        -- | Convenience function to turn RATargets into a triple of monoids, which helps
        --   in the Semigroup definition.
        toMonoidTriple
            :: RATargets
            -> (Last Target, Last ReinstallAtomsTarget, Maybe (NESet CustomTarget))
        toMonoidTriple = bifoldMap
            (\t -> (pure t, mempty, mempty))
            (bifoldMap
                (\r -> (mempty, pure r, mempty))
                (\s -> (mempty, mempty, pure s))
            ) . unRATargets

-- | Modes that are available for portage. This includes the basic
--   'PortageBasicMode' and 'PortageListMode' (which correspond to
--   'BasicMode' and 'ListMode' respectively), but also 'ReinstallAtomsMode',
--   which is specific to portage.
data PortageMode
    = PortageBasicMode (Either PortageBasicTarget Target)
    | PortageListMode Target
    | ReinstallAtomsMode RATargets
    deriving (Eq, Ord, Show)

-- | Extra targets that are available for portage in @--mode=basic@. Currently,
--   this only includes @@preserved-rebuild@.
data PortageBasicTarget = PreservedRebuild
    deriving (Eq, Ord, Show)

-- | Targets that are availble only for @--mode=reinstall-atoms' (portage
--   only). This includes 'WorldTarget' (@@world@) and 'WorldFullTarget', a
--   special convenience target which adds @@world@ to the target list and
--   also passes @--newuse --with-bdeps=y@ to portage for convenience.
--   (See 'getExtraRawArgs').
data ReinstallAtomsTarget
    = WorldTarget
    | WorldFullTarget
    deriving (Eq, Ord, Show)

-- | Extract the t'RunMode' (or 'PortageMode') from the 'HUMode' ADT.
--
--   Like other functions in this module, it takes a 'PkgManager' (as opposed
--   to 'HUMode'), since only the v'RunMode' constructor of 'HUMode' is
--   relevant.
runMode :: PkgManager -> Either RunMode PortageMode
runMode (Portage rm) = Right rm
runMode (PkgCore rm) = Left rm
runMode (Paludis rm) = Left rm
runMode (CustomPM _ rm) = Left rm

-- | Extract the 'Target' from a t'RunMode' constructor.
getTarget :: RunMode -> Target
getTarget (BasicMode t) = t
getTarget (ListMode t) = t

-- | Convert from the @haskell-updater@ ADT to 'LoopMode', which encodes
--   how the looping mechanism of @haskell-updater@ should funciton.
--
--   Like other functions in this module, it takes a 'PkgManager' (as opposed
--   to 'HUMode'), since only the v'RunMode' constructor of 'HUMode' is
--   relevant.
getLoopType :: RunModifier -> PkgManager -> LoopType
getLoopType rm
      -- Always use NoLoop if --pretend is passed on the command line
    | any (== PretendBuild) (flags rm) = const NoLoop
    | otherwise = \case
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

-- | Convert from the 'HUMode' ADT to 'ExtraRawArgs', hard-coded extra
--   arguments that will be passed to the package manager.
--
--   Like other functions in this module, it takes a 'PkgManager' (as opposed
--   to 'HUMode'), since only the v'RunMode' constructor of 'HUMode' is
--   relevant.
getExtraRawArgs :: PkgManager -> ExtraRawArgs
getExtraRawArgs = ExtraRawArgs . \case
    Portage (ReinstallAtomsMode (RATargets t)) ->
        bifoldMap (const []) (bifoldMap fromRAT (const [])) t
    _ -> []
  where
    fromRAT = \case
        WorldFullTarget -> ["--newuse", "--with-bdeps=y"]
        WorldTarget -> []
