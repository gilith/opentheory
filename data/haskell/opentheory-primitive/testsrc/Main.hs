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

import qualified OpenTheory.Primitive.Natural as Natural
import OpenTheory.Primitive.Test

assertion0 :: Bool
assertion0 = True

proposition0 :: Bool -> Bool
proposition0 p =
  p || not p

proposition1 :: Natural.Natural -> Natural.Natural -> Bool
proposition1 m n =
  m + n == n + m

main :: IO ()
main =
    do assert "Assertion 0:\n  T\n  " assertion0
       check "Proposition 0:\n  !p. p \\/ ~p\n  " proposition0
       check "Proposition 1:\n  !m n. m + n = n + m\n  " proposition1
       return ()
