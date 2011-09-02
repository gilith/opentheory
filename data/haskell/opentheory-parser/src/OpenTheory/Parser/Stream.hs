{- |
Module: $Header$
Description: Basic parsers in Haskell
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module OpenTheory.Parser.Stream
where

data Stream a =
    Error
  | Eof
  | Stream a (Stream a)

append :: [a] -> Stream a -> Stream a
append [] s = s
append (h:t) s = Stream h (append t s)

fromList :: [a] -> Stream a
fromList l = append l Eof

length :: Stream a -> OpenTheory.Number.Natural.Natural
length Error = 0
length Eof = 0
length (Stream a s) = OpenTheory.Number.Natural.suc (length s)

toList :: Stream a -> Maybe [a]
toList Error = Nothing
toList Eof = Just []
toList (Stream a s) =
  case toList s of Nothing -> Nothing | Just l -> Just (a:l)
