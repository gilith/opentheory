{- |
module: $Header$
description: The Haskell base
license: MIT

maintainer: Joe Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Data.Byte.Bits
where

import qualified OpenTheory.Primitive.Byte as Primitive.Byte

toByte :: [Bool] -> Primitive.Byte.Byte
toByte [] = 0
toByte (h : t) =
  if h then Primitive.Byte.shiftLeft (toByte t) 1 + 1
  else Primitive.Byte.shiftLeft (toByte t) 1
