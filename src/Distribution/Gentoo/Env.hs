{- |
   Module      : Distribution.Gentoo.Env
   Description : Global environment for haskell-updater

   This module contains a representation of the global environment for
   @haskell-updater@, which is parsed from the command line.
 -}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Gentoo.Env
    ( EnvT(runEnvT)
    , HasRunModifier(..)
    , HasPkgManager(..)
    , askLoopType
    , askExtraRawArgs
    , HasRawPMArgs(..)
    ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Proxy
import System.IO (hPutStrLn, stderr)

import Distribution.Gentoo.Types
import Distribution.Gentoo.Types.HUMode
import Output

type Env = (RunModifier, PkgManager, RawPMArgs)

newtype EnvT m a = EnvT
    { runEnvT :: Env -> m a }
    deriving stock Functor
    deriving (Applicative, Monad, MonadIO, MonadReader Env) via ReaderT Env m
    deriving MonadTrans via ReaderT Env

instance MonadIO m => MonadSay (EnvT m) where
    outputLn = liftIO . hPutStrLn stderr
    askVerbosity = asks $ \(rm, _, _) -> verbosity rm

instance MonadExit m => MonadExit (EnvT m) where
    type ExitArg (EnvT m) = ExitArg m
    success = lift . success
    die = lift . die
    exitWith = lift . exitWith
    isSuccess (_ :: Proxy (EnvT m)) = isSuccess (Proxy :: Proxy m)

class Monad m => HasRunModifier m where
    askRunModifier :: m RunModifier

instance Monad m => HasRunModifier (EnvT m) where
    askRunModifier = asks $ \(rm, _, _) -> rm

class Monad m => HasPkgManager m where
    askPkgManager :: m PkgManager

instance Monad m => HasPkgManager (EnvT m) where
    askPkgManager = asks $ \(_, pm, _) -> pm

instance HasPkgManager m => HasPkgManager (StateT s m) where
    askPkgManager = lift askPkgManager

instance HasPkgManager m => HasPkgManager (ReaderT r m) where
    askPkgManager = lift askPkgManager

askLoopType :: HasPkgManager m => m LoopType
askLoopType = getLoopType <$> askPkgManager

askExtraRawArgs :: HasPkgManager m => m ExtraRawArgs
askExtraRawArgs = getExtraRawArgs <$> askPkgManager

class Monad m => HasRawPMArgs m where
    askRawPMArgs :: m RawPMArgs

instance Monad m => HasRawPMArgs (EnvT m) where
    askRawPMArgs = asks $ \(_, _, rawArgs) -> rawArgs

instance HasRawPMArgs m => HasRawPMArgs (StateT s m) where
    askRawPMArgs = lift askRawPMArgs

instance HasRawPMArgs m => HasRawPMArgs (ReaderT r m) where
    askRawPMArgs = lift askRawPMArgs
