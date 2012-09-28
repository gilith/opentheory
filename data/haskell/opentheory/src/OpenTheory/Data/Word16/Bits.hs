{- |
module: $Header$
description: The Haskell base
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Data.Word16.Bits
where

import qualified OpenTheory.Primitive.Word16 as Primitive.Word16

toWord16 :: [Bool] -> Primitive.Word16.Word16
toWord16 [] = 0
toWord16 (h : t) =
  if h then Primitive.Word16.shiftLeft (toWord16 t) 1 + 1
  else Primitive.Word16.shiftLeft (toWord16 t) 1
