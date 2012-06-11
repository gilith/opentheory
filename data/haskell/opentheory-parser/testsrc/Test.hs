{- |
Module: Main
Description: Simple stream parsers - testing
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module Main
  ( main )
where

import qualified OpenTheory.Data.List
import qualified OpenTheory.Data.Option
import qualified OpenTheory.Number.Natural
import qualified OpenTheory.Parser.Stream
import qualified OpenTheory.Primitive.Probability.Random
import qualified OpenTheory.Primitive.Test

prop0 :: OpenTheory.Primitive.Probability.Random.Random -> Bool
prop0 r =
  let (l, _) =
      OpenTheory.Data.List.fromRandom OpenTheory.Number.Natural.fromRandom
        r in
  OpenTheory.Data.Option.equal (OpenTheory.Data.List.equal ==)
    (OpenTheory.Parser.Stream.toList (OpenTheory.Parser.Stream.fromList l))
    (Just l)

prop1 :: OpenTheory.Primitive.Probability.Random.Random -> Bool
prop1 r =
  let (l, r') =
      OpenTheory.Data.List.fromRandom OpenTheory.Number.Natural.fromRandom
        r in
  let (s, _) =
      OpenTheory.Parser.Stream.fromRandom
        OpenTheory.Number.Natural.fromRandom r' in
  OpenTheory.Parser.Stream.size (OpenTheory.Parser.Stream.append l s) ==
  OpenTheory.Data.List.size l + OpenTheory.Parser.Stream.size s

main :: IO ()
main =
    do OpenTheory.Primitive.Test.check "Proposition 0:\n  !r.\n    let (l, r') <-\n        Haskell.Data.List.fromRandom Haskell.Number.Natural.fromRandom r in\n    Haskell.Data.Option.equal (Haskell.Data.List.equal (=))\n      (Stream.toList (Stream.fromList l)) (some l)\n  " prop0
       OpenTheory.Primitive.Test.check "Proposition 1:\n  !r.\n    let (l, r') <-\n        Haskell.Data.List.fromRandom Haskell.Number.Natural.fromRandom r in\n    let (s, r'') <-\n        Stream.fromRandom Haskell.Number.Natural.fromRandom r' in\n    Stream.size (Stream.append l s) =\n    Haskell.Data.List.size l + Stream.size s\n  " prop1
       return ()
