{- |
Module: Main
Description: Prime numbers - testing
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module Main
  ( main )
where

import qualified OpenTheory.Data.Stream as Data.Stream
import qualified OpenTheory.Number.Natural.Geometric
  as Number.Natural.Geometric
import qualified OpenTheory.Number.Natural.Prime as Number.Natural.Prime
import qualified OpenTheory.Primitive.Random as Primitive.Random
import qualified OpenTheory.Primitive.Test as Primitive.Test

proposition0 :: Primitive.Random.Random -> Bool
proposition0 r =
  let (i, r') = Number.Natural.Geometric.fromRandom r in
  let (j, _) = Number.Natural.Geometric.fromRandom r' in
  (Data.Stream.nth Number.Natural.Prime.all i <=
   Data.Stream.nth Number.Natural.Prime.all j) == (i <= j)

proposition1 :: Primitive.Random.Random -> Bool
proposition1 r =
  let (i, r') = Number.Natural.Geometric.fromRandom r in
  let (j, _) = Number.Natural.Geometric.fromRandom r' in
  0 <
  Data.Stream.nth Number.Natural.Prime.all (i + j + 1) `mod`
  Data.Stream.nth Number.Natural.Prime.all i

main :: IO ()
main =
    do Primitive.Test.check "Proposition 0:\n  !r.\n    let (i, r') <- H.Geometric.fromRandom r in\n    let (j, r'') <- H.Geometric.fromRandom r' in\n    H.nth H.Prime.all i <= H.nth H.Prime.all j <=> i <= j\n  " proposition0
       Primitive.Test.check "Proposition 1:\n  !r.\n    let (i, r') <- H.Geometric.fromRandom r in\n    let (j, r'') <- H.Geometric.fromRandom r' in\n    0 < H.nth H.Prime.all (i + j + 1) mod H.nth H.Prime.all i\n  " proposition1
       return ()
