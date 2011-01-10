{- |
Module: $Header$
Description: OpenTheory QuickCheck interface
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

OpenTheory QuickCheck interface
-}
module OpenTheory.Test
  ( check )
where

import Test.QuickCheck

checkArgs :: Test.QuickCheck.Args
checkArgs = Test.QuickCheck.stdArgs { maxSuccess = 100 }

check :: Testable prop => String -> prop -> IO ()
check name prop =
  do putStr (name ++ ": ")
     Test.QuickCheck.quickCheckWith checkArgs prop
