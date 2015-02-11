{- |
Module: $Header$
Description: OpenTheory QuickCheck interface
License: MIT

Maintainer: Joe Leslie-Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

OpenTheory QuickCheck interface
-}
module Main
  ( main )
where

import qualified OpenTheory.Primitive.Random as Primitive.Random
import qualified OpenTheory.Primitive.Test as Primitive.Test

assertion0 :: Bool
assertion0 = True

proposition0 :: Primitive.Random.Random -> Bool
proposition0 r =
  let (p,_) = Primitive.Random.bit r in
  p || not p

proposition1 :: Integer -> Integer -> Bool
proposition1 m n =
  m + n == n + m

main :: IO ()
main =
    do Primitive.Test.assert "Assertion 0:\n  T\n  " assertion0
       Primitive.Test.check "Proposition 0:\n  !p. p \\/ ~p\n  " proposition0
       Primitive.Test.check "Proposition 1:\n  !m n. m + n = n + m\n  " proposition1
       return ()
