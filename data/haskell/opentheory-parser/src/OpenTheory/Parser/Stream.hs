{- |
module: $Header$
description: Stream parsers
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.Parser.Stream
where

import qualified OpenTheory.Primitive.Natural as Natural
import qualified Test.QuickCheck as QuickCheck

data Stream a =
    Error
  | Eof
  | Cons a (Stream a)
  deriving (Eq, Ord, Show)

append :: [a] -> Stream a -> Stream a
append [] xs = xs
append (h : t) xs = Cons h (append t xs)

fromList :: [a] -> Stream a
fromList l = append l Eof

lengthStream :: Stream a -> Natural.Natural
lengthStream Error = 0
lengthStream Eof = 0
lengthStream (Cons _ xs) = lengthStream xs + 1

mapStream :: (a -> b) -> Stream a -> Stream b
mapStream _ Error = Error
mapStream _ Eof = Eof
mapStream f (Cons x xs) = Cons (f x) (mapStream f xs)

toList :: Stream a -> ([a], Bool)
toList Error = ([], True)
toList Eof = ([], False)
toList (Cons x xs) = let (l, e) = toList xs in (x : l, e)

instance QuickCheck.Arbitrary a => QuickCheck.Arbitrary (Stream a) where
  arbitrary =
    fmap (\(l, b) -> append l (if b then Error else Eof))
      QuickCheck.arbitrary
