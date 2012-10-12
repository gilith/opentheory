{- |
module: Main
description: Prime numbers - testing
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified OpenTheory.Data.Stream as Data.Stream
import qualified OpenTheory.Number.Natural as Number.Natural
import qualified OpenTheory.Number.Natural.Geometric
  as Number.Natural.Geometric
import qualified OpenTheory.Number.Natural.Prime as Number.Natural.Prime
import qualified OpenTheory.Primitive.Random as Primitive.Random
import qualified OpenTheory.Primitive.Test as Primitive.Test

assertion0 :: Bool
assertion0 = not (Data.Stream.nth Number.Natural.Prime.all 0 == 0)

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
  not
    (Number.Natural.divides (Data.Stream.nth Number.Natural.Prime.all i)
       (Data.Stream.nth Number.Natural.Prime.all (i + j + 1)))

main :: IO ()
main =
    do Primitive.Test.assert "Assertion 0:\n  ~(H.nth H.Prime.all 0 = 0)\n  " assertion0
       Primitive.Test.check "Proposition 0:\n  !r.\n    let (i, r') <- H.Geometric.fromRandom r in\n    let (j, r'') <- H.Geometric.fromRandom r' in\n    H.nth H.Prime.all i <= H.nth H.Prime.all j <=> i <= j\n  " proposition0
       Primitive.Test.check "Proposition 1:\n  !r.\n    let (i, r') <- H.Geometric.fromRandom r in\n    let (j, r'') <- H.Geometric.fromRandom r' in\n    ~H.divides (H.nth H.Prime.all i) (H.nth H.Prime.all (i + j + 1))\n  " proposition1
       return ()
