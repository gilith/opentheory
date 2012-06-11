{- |
Module: $Header$
Description: Simple stream parsers
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module OpenTheory.Parser.Stream
where

import qualified OpenTheory.Data.List
import qualified OpenTheory.Primitive.Number.Natural
import qualified OpenTheory.Primitive.Probability.Random

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
  (OpenTheory.Primitive.Probability.Random.Random ->
     (a, OpenTheory.Primitive.Probability.Random.Random)) ->
    OpenTheory.Primitive.Probability.Random.Random ->
    (Stream a, OpenTheory.Primitive.Probability.Random.Random)
fromRandom d r =
  let (l, r') = OpenTheory.Data.List.fromRandom d r in
  let (b, r'') = OpenTheory.Primitive.Probability.Random.bit r' in
  (append l (if b then Error else Eof), r'')

size :: Stream a -> OpenTheory.Primitive.Number.Natural.Natural
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
