{- |
module: Main
description: Fibonacci numbers - testing
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified OpenTheory.Natural.Fibonacci as Fibonacci
import qualified OpenTheory.Primitive.Natural as Natural
import qualified OpenTheory.Stream as Stream
import OpenTheory.Primitive.Test

proposition0 :: Natural.Natural -> Bool
proposition0 n = Fibonacci.zeckendorf (Fibonacci.encode n)

proposition1 :: Natural.Natural -> Bool
proposition1 n = Fibonacci.decode (Fibonacci.encode n) == n

proposition2 :: Natural.Natural -> Natural.Natural -> Bool
proposition2 j k =
  Stream.nth Fibonacci.fibonaccis (j + (k + 1)) ==
  Stream.nth Fibonacci.fibonaccis j * Stream.nth Fibonacci.fibonaccis k +
  Stream.nth Fibonacci.fibonaccis (j + 1) *
  Stream.nth Fibonacci.fibonaccis (k + 1)

main :: IO ()
main =
    do check "Proposition 0:\n  !n. zeckendorf (encode n)\n  " proposition0
       check "Proposition 1:\n  !n. decode (encode n) = n\n  " proposition1
       check "Proposition 2:\n  !j k.\n    nth all (j + (k + 1)) =\n    nth all j * nth all k + nth all (j + 1) * nth all (k + 1)\n  " proposition2
       return ()
