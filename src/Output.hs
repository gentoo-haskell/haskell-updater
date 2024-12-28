{- |
   Module      : Main
   Description : The haskell-updater executable
   License     : GPL-2 or later

   Fancy output facility.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Output (
                pkgListPrintLn
              , printList
              , MonadSay(..)
              , say
              , vsay
              , SayIO(..)
              , Verbosity(..)
              ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Set as Set
import System.IO (hPutStrLn, stderr)

import Distribution.Gentoo.Packages

-- output mode (chattiness)
data Verbosity = Quiet
               | Normal
               | Verbose
     deriving (Eq, Ord, Show, Read)

-- | Monads that have an environment that stores the specified verbosity level,
--   and can output messages
class Monad m => MonadSay m where
    outputLn :: String -> m () -- ^ Output a line
    askVerbosity :: m Verbosity

say :: MonadSay m => String -> m ()
say msg = askVerbosity >>= \case
        Quiet   -> return ()
        Normal  -> outputLn msg
        Verbose -> outputLn msg

vsay :: MonadSay m => String -> m ()
vsay msg = askVerbosity >>= \case
        Quiet   -> return ()
        Normal  -> return ()
        Verbose -> outputLn msg

-- Print a bullet list of values with one value per line.
printList :: MonadSay m => (a -> String) -> [a] -> m ()
printList f = mapM_ (say . (++) "  * " . f)

-- Print a list of packages, with a description of what they are.
pkgListPrintLn :: MonadSay m => String -> Set.Set Package -> m ()
pkgListPrintLn desc pkgs
    | null pkgs = do
        say $ unwords ["No", desc, "packages found!"]
        say ""
    | otherwise = askVerbosity >>= \case
        Quiet -> pure ()
        Normal -> do
            outputLn $ unwords
                ["Found", show (Set.size pkgs), desc, "packages."]
            outputLn ""
        Verbose -> do
            outputLn $ unwords
                ["Found the following", desc, "packages:"]
            printList printPkg (Set.toList pkgs)
            outputLn ""

-- | A simple wrapper for adding a basic 'MonadSay' instance to 'IO'.
--   This always uses 'Normal' verbosity.
--
--   Note that we avoid directly adding a 'MonadSay' instance for 'IO', since
--   this can cause the type-checker to miss certain mistakes (such as using
--   @'liftIO' . 'vsay'@, which would choose the 'IO' instance, thus always
--   using 'Normal' verbosity and ignoring the 'Verbosity' given via the
--   command line).
newtype SayIO a = SayIO { sayIO :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadSay SayIO where
    outputLn = SayIO . hPutStrLn stderr
    askVerbosity = pure Normal

instance MonadSay m => MonadSay (StateT s m) where
    outputLn = lift . outputLn
    askVerbosity = lift askVerbosity

instance MonadSay m => MonadSay (ReaderT r m) where
    outputLn = lift . outputLn
    askVerbosity = lift askVerbosity
