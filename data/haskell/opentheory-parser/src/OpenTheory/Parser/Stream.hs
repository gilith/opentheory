{- |
module: $Header$
description: Simple stream parsers
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Parser.Stream
where

import qualified OpenTheory.Data.List.Geometric as Data.List.Geometric
import qualified OpenTheory.Primitive.Natural as Primitive.Natural
import qualified OpenTheory.Primitive.Random as Primitive.Random

data Stream a =
    Error
  | Eof
  | Cons a (Stream a)

append :: [a] -> Stream a -> Stream a
append [] s = s
append (h : t) s = Cons h (append t s)

fromList :: [a] -> Stream a
fromList l = append l Eof

fromRandom ::
  (Primitive.Random.Random -> (a, Primitive.Random.Random)) ->
    Primitive.Random.Random -> (Stream a, Primitive.Random.Random)
fromRandom d r =
  let (l, r') = Data.List.Geometric.fromRandom d r in
  let (b, r'') = Primitive.Random.bit r' in
  (append l (if b then Error else Eof), r'')

size :: Stream a -> Primitive.Natural.Natural
size Error = 0
size Eof = 0
size (Cons _ s) = size s + 1

toList :: Stream a -> Maybe [a]
toList Error = Nothing
toList Eof = Just []
toList (Cons a s) =
  case toList s of
    Nothing -> Nothing
    Just l -> Just (a : l)
