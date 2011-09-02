{- |
Module: $Header$
Description: Basic parsers in Haskell
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module OpenTheory.Parser
where



parse :: Parser a b -> Stream.Stream a -> Maybe (b, Stream.Stream a)
parse p Stream.Error = Nothing
parse p Stream.Eof = Nothing
parse p (Stream.Stream a s) = unParser p a s
