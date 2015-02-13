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


data Stream a =
    Error
  | Eof
  | Cons a (Stream a)
