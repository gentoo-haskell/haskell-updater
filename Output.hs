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
              , Verbosity(..)
              ) where

import System.IO (hPutStrLn, stderr)

import Distribution.Gentoo.Packages

-- output mode (chattiness)
data Verbosity = Normal
               | Quiet
     deriving (Eq, Ord, Show, Read)

say :: Verbosity -> String -> IO ()
say Normal msg = hPutStrLn stderr msg
say Quiet _msg = return ()

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
