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
import OpenTheory.Primitive.Test

proposition0 :: Natural.Natural -> Bool
proposition0 n = Fibonacci.zeckendorf (Fibonacci.encode n)

proposition1 :: Natural.Natural -> Bool
proposition1 n = Fibonacci.decode (Fibonacci.encode n) == n

main :: IO ()
main =
    do check "Proposition 0:\n  !n. zeckendorf (encode n)\n  " proposition0
       check "Proposition 1:\n  !n. decode (encode n) = n\n  " proposition1
       return ()
