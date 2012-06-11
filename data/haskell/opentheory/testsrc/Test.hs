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

import qualified OpenTheory.Number.Natural
import qualified OpenTheory.Primitive.Probability.Random
import qualified OpenTheory.Primitive.Test

prop0 :: OpenTheory.Primitive.Probability.Random.Random -> Bool
prop0 r =
  let (n1, r') = OpenTheory.Number.Natural.fromRandom r in
  let (n2, _) = OpenTheory.Number.Natural.fromRandom r' in
  not (n1 == n2) || n2 == n1

main :: IO ()
main =
    do OpenTheory.Primitive.Test.check "Proposition 0:\n  !r.\n    let (n1, r') <- Number.Natural.fromRandom r in\n    let (n2, r'') <- Number.Natural.fromRandom r' in\n    ~(n1 = n2) \\/ n2 = n1\n  " prop0
       return ()
