{- |
module: Main
description: Prime natural numbers - testing
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified OpenTheory.Natural.Divides as Divides
import qualified OpenTheory.Natural.Prime as Prime
import qualified OpenTheory.Primitive.Natural as Natural
import qualified OpenTheory.Stream as Stream
import OpenTheory.Primitive.Test

assertion0 :: Bool
assertion0 = not (Stream.nth Prime.primes 0 == 0)

proposition0 :: Natural.Natural -> Natural.Natural -> Bool
proposition0 i j =
  (Stream.nth Prime.primes i <= Stream.nth Prime.primes j) == (i <= j)

proposition1 :: Natural.Natural -> Natural.Natural -> Bool
proposition1 i j =
  not
    (Divides.divides (Stream.nth Prime.primes i)
       (Stream.nth Prime.primes (i + (j + 1))))

proposition2 :: Natural.Natural -> Natural.Natural -> Bool
proposition2 n i =
  any (\p -> Divides.divides p (n + 2))
    (Stream.naturalTake Prime.primes i) ||
  Stream.nth Prime.primes i <= n + 2

main :: IO ()
main =
    do assert "Assertion 0:\n  ~(nth Prime.all 0 = 0)\n  " assertion0
       check "Proposition 0:\n  !i j. nth Prime.all i <= nth Prime.all j <=> i <= j\n  " proposition0
       check "Proposition 1:\n  !i j. ~divides (nth Prime.all i) (nth Prime.all (i + (j + 1)))\n  " proposition1
       check "Proposition 2:\n  !n i.\n    any (\\p. divides p (n + 2)) (take Prime.all i) \\/\n    nth Prime.all i <= n + 2\n  " proposition2
       return ()
