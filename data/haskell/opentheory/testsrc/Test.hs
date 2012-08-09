{- |
Module: Main
Description: The Haskell base - testing
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module Main
  ( main )
where

import qualified OpenTheory.Number.Natural as Number.Natural
import qualified OpenTheory.Primitive.Random as Primitive.Random
import qualified OpenTheory.Primitive.Test as Primitive.Test

proposition0 :: Primitive.Random.Random -> Bool
proposition0 r =
  let (n1, r') = Number.Natural.fromRandom r in
  let (n2, _) = Number.Natural.fromRandom r' in
  not (n1 == n2) || n2 == n1

main :: IO ()
main =
    do Primitive.Test.check "Proposition 0:\n  !r.\n    let (n1, r') <- H.fromRandom r in\n    let (n2, r'') <- H.fromRandom r' in\n    ~(n1 = n2) \\/ n2 = n1\n  " proposition0
       return ()
