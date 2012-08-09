{- |
Module: $Header$
Description: The Haskell base
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module OpenTheory.Data.Word16.Bits
where

import qualified OpenTheory.Primitive.Word16 as Primitive.Word16

toWord16 :: [Bool] -> Primitive.Word16.Word16
toWord16 [] = 0
toWord16 (h : t) =
  if h then Primitive.Word16.shiftLeft (toWord16 t) 1 + 1
  else Primitive.Word16.shiftLeft (toWord16 t) 1
