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

import qualified OpenTheory.Number.Natural

data Stream a =
    Error
  | Eof
  | Stream a (Stream a)

append :: [a] -> Stream a -> Stream a
append [] s = s
append (h : t) s = Stream h (append t s)

fromList :: [a] -> Stream a
fromList l = append l Eof

size :: Stream a -> OpenTheory.Number.Natural.Natural
size Error = 0
size Eof = 0
size (Stream _ s) = size s + 1

toList :: Stream a -> Maybe [a]
toList Error = Nothing
toList Eof = Just []
toList (Stream a s) =
  case toList s of
    Nothing -> Nothing
    Just l -> Just (a : l)
