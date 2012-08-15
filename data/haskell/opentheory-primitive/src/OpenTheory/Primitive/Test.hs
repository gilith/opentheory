{- |
module: $Header$
description: OpenTheory QuickCheck interface
license: MIT

maintainer: Joe Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Primitive.Test
  ( check )
where

import Test.QuickCheck

checkArgs :: Test.QuickCheck.Args
checkArgs = Test.QuickCheck.stdArgs { maxSuccess = 100 }

check :: Testable prop => String -> prop -> IO ()
check desc prop =
  do putStr desc
     Test.QuickCheck.quickCheckWith checkArgs prop
