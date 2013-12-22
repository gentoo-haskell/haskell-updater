{- |
   Module      : Main
   Description : The haskell-updater executable
   License     : GPL-2 or later

   Fancy output facility.
-}
module Output (
                pkgListPrint
              , printList
              , say
              , vsay
              , Verbosity(..)
              ) where

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
pkgListPrint :: Verbosity -> String -> [Package] -> IO ()
pkgListPrint v desc pkgs
    = if null pkgs
      then say v $ unwords ["No", desc, "packages found!\n"]
      else do say v $ unwords ["Found the following"
                              , desc, "packages:"]
              printList v printPkg pkgs
              say v ""
