{- |
   Module      : Main
   Description : The haskell-updater executable
   License     : GPL-2 or later

   Fancy output facility.
-}
module Output (
                pkgListPrintLn
              , printList
              , say
              , vsay
              , Verbosity(..)
              ) where

import qualified Data.Set as Set
import System.IO (hPutStrLn, stderr)

import Distribution.Gentoo.Packages

-- output mode (chattiness)
data Verbosity = Quiet
               | Normal
               | Verbose
     deriving (Eq, Ord, Show, Read)

say :: Verbosity -> String -> IO ()
say verb_l msg =
    case verb_l of
        Quiet   -> return ()
        Normal  -> hPutStrLn stderr msg
        Verbose -> hPutStrLn stderr msg

vsay :: Verbosity -> String -> IO ()
vsay verb_l msg =
    case verb_l of
        Quiet   -> return ()
        Normal  -> return ()
        Verbose -> hPutStrLn stderr msg

-- Print a bullet list of values with one value per line.
printList :: Verbosity -> (a -> String) -> [a] -> IO ()
printList v f = mapM_ (say v . (++) "  * " . f)

-- Print a list of packages, with a description of what they are.
pkgListPrintLn :: Verbosity -> String -> Set.Set Package -> IO ()
pkgListPrintLn v desc pkgs
    | null pkgs = do
        say v $ unwords ["No", desc, "packages found!"]
        say v ""
    | otherwise = case v of
        Quiet -> pure ()
        Normal -> do
            hPutStrLn stderr $ unwords
                ["Found", show (Set.size pkgs), desc, "packages."]
            hPutStrLn stderr ""
        Verbose -> do
            hPutStrLn stderr $ unwords
                ["Found the following", desc, "packages:"]
            printList v printPkg (Set.toList pkgs)
            hPutStrLn stderr ""
