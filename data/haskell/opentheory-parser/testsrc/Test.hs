{- |
module: Main
description: Stream parsers - testing
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified OpenTheory.Data.List as Data.List
import qualified OpenTheory.Data.List.Geometric as Data.List.Geometric
import qualified OpenTheory.Data.Option as Data.Option
import qualified OpenTheory.Number.Natural as Number.Natural
import qualified OpenTheory.Parser.Stream as Parser.Stream
import qualified OpenTheory.Primitive.Random as Primitive.Random
import qualified OpenTheory.Primitive.Test as Primitive.Test

proposition0 :: Primitive.Random.Random -> Bool
proposition0 r =
  let (l, _) =
        Data.List.Geometric.fromRandom Number.Natural.fromRandom r in
  Parser.Stream.size (Parser.Stream.fromList l) == Data.List.size l

proposition1 :: Primitive.Random.Random -> Bool
proposition1 r =
  let (l, _) =
        Data.List.Geometric.fromRandom Number.Natural.fromRandom r in
  Data.Option.equal (Data.List.equal (==))
    (Parser.Stream.toList (Parser.Stream.fromList l)) (Just l)

proposition2 :: Primitive.Random.Random -> Bool
proposition2 r =
  let (s, _) = Parser.Stream.fromRandom Number.Natural.fromRandom r in
  case Parser.Stream.toList s of
    Nothing -> True
    Just l -> Data.List.size l == Parser.Stream.size s

proposition3 :: Primitive.Random.Random -> Bool
proposition3 r =
  let (l, r') =
        Data.List.Geometric.fromRandom Number.Natural.fromRandom r in
  let (s, _) = Parser.Stream.fromRandom Number.Natural.fromRandom r' in
  Parser.Stream.size (Parser.Stream.append l s) ==
  Data.List.size l + Parser.Stream.size s

proposition4 :: Primitive.Random.Random -> Bool
proposition4 r =
  let (l, r') =
        Data.List.Geometric.fromRandom Number.Natural.fromRandom r in
  let (s, _) = Parser.Stream.fromRandom Number.Natural.fromRandom r' in
  Data.Option.equal (Data.List.equal (==))
    (Parser.Stream.toList (Parser.Stream.append l s))
    (case Parser.Stream.toList s of
       Nothing -> Nothing
       Just ls -> Just (l ++ ls))

main :: IO ()
main =
    do Primitive.Test.check "Proposition 0:\n  !r.\n    let (l, r') <- H.Geometric.fromRandom H.fromRandom r in\n    H.Stream.size (H.Stream.fromList l) = H.size l\n  " proposition0
       Primitive.Test.check "Proposition 1:\n  !r.\n    let (l, r') <- H.Geometric.fromRandom H.fromRandom r in\n    H.equal (H.equal (=)) (H.Stream.toList (H.Stream.fromList l)) (some l)\n  " proposition1
       Primitive.Test.check "Proposition 2:\n  !r.\n    let (s, r') <- H.Stream.fromRandom H.fromRandom r in\n    case H.Stream.toList s of\n      none -> T\n    | some l -> H.size l = H.Stream.size s\n  " proposition2
       Primitive.Test.check "Proposition 3:\n  !r.\n    let (l, r') <- H.Geometric.fromRandom H.fromRandom r in\n    let (s, r'') <- H.Stream.fromRandom H.fromRandom r' in\n    H.Stream.size (H.Stream.append l s) = H.size l + H.Stream.size s\n  " proposition3
       Primitive.Test.check "Proposition 4:\n  !r.\n    let (l, r') <- H.Geometric.fromRandom H.fromRandom r in\n    let (s, r'') <- H.Stream.fromRandom H.fromRandom r' in\n    H.equal (H.equal (=)) (H.Stream.toList (H.Stream.append l s))\n      (case H.Stream.toList s of none -> none | some ls -> some (l @ ls))\n  " proposition4
       return ()
