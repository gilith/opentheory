{- |
Module: $Header$
Description: Testing the OpenTheory standard theory library
License: MIT

Maintainer: Joe Leslie-Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module Main
  ( main )
where

import OpenTheory.Primitive.Natural
import OpenTheory.Primitive.Test

assertion0 :: Bool
assertion0 = True

assertion1 :: Bool
assertion1 = (2 :: Natural) + 2 == 4

proposition0 :: Bool -> Bool
proposition0 p = p || not p

proposition1 :: Natural -> Natural -> Bool
proposition1 m n = m + n == n + m

proposition2 :: Natural -> Natural -> Bool
proposition2 n k = shiftLeft n k == (2 ^ k) * n

proposition3 :: Natural -> Natural -> Bool
proposition3 n k = shiftRight n k == n `div` (2 ^ k)

main :: IO ()
main =
    do assert "Assertion 0:\n  T\n  " assertion0
       assert "Assertion 1:\n  2 + 2 = 4\n  " assertion1
       check "Proposition 0:\n  !p. p \\/ ~p\n  " proposition0
       check "Proposition 1:\n  !m n. m + n = n + m\n  " proposition1
       check "Proposition 2:\n  !n k. shiftLeft n k = 2 ^ k * n\n  " proposition2
       check "Proposition 3:\n  !n k. shiftRight n k = n div 2 ^ k\n  " proposition3
       return ()
