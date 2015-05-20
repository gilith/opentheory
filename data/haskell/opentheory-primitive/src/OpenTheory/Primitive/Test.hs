{- |
module: $Header$
description: OpenTheory QuickCheck interface
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Primitive.Test
  ( assert,
    check )
where

import Test.QuickCheck

assert :: String -> Bool -> IO ()
assert desc prop =
  do putStr desc
     if prop
       then putStrLn "+++ OK"
       else
         do putStr "**"
            putStrLn "* Failed!"
            error "Assertion failed"

checkArgs :: Test.QuickCheck.Args
checkArgs = Test.QuickCheck.stdArgs { maxSuccess = 100 }

check :: Testable prop => String -> prop -> IO ()
check desc prop =
  do putStr desc
     res <- Test.QuickCheck.quickCheckWithResult checkArgs prop
     case res of
       Test.QuickCheck.Failure {} -> error "Proposition failed"
       _ -> return ()
