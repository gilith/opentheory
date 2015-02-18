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

data Stream a =
    Error
  | Eof
  | Cons a (Stream a)

append :: [a] -> Stream a -> Stream a
append [] s = s
append (h : t) s = Cons h (append t s)

fromList :: [a] -> Stream a
fromList l = append l Eof

lengthStream :: Stream a -> Natural.Natural
lengthStream Error = 0
lengthStream Eof = 0
lengthStream (Cons _ s) = lengthStream s + 1

mapStream :: (a -> b) -> Stream a -> Stream b
mapStream _ Error = Error
mapStream _ Eof = Eof
mapStream f (Cons a s) = Cons (f a) (mapStream f s)

toList :: Stream a -> Maybe [a]
toList Error = Nothing
toList Eof = Just []
toList (Cons a s) =
  case toList s of
    Nothing -> Nothing
    Just l -> Just (a : l)
