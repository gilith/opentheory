{- |
Module: $Header$
Description: OpenTheory QuickCheck interface
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

OpenTheory QuickCheck interface
-}
module Main
  ( main )
where

import qualified Data.Word
import qualified OpenTheory.Primitive.Data.Word16
import qualified OpenTheory.Primitive.Test

prop0 :: Data.Word.Word16 -> Bool
prop0 w =
  let (b1,b2) = OpenTheory.Primitive.Data.Word16.toBytes w in
  OpenTheory.Primitive.Data.Word16.fromBytes b1 b2 == w

main :: IO ()
main =
    do OpenTheory.Primitive.Test.check "prop0" prop0
       return ()
