{- |
Module: $Header$
Description: A verified UTF8 parser
License: MIT

License-file: LICENSE
Maintainer: Joe Hurd
-}
module Main
  ( main )
where

import qualified Data.Word
import qualified OpenTheory.Char
import qualified OpenTheory.Test

prop1 :: [OpenTheory.Char.Unicode] -> Bool
prop1 cs =
    case OpenTheory.Char.decode (OpenTheory.Char.encode cs) of
      Just cs' -> cs == cs'
      Nothing -> False

prop2 :: [Data.Word.Word8] -> Bool
prop2 bs =
    case OpenTheory.Char.decode bs of
      Just cs -> OpenTheory.Char.encode cs == bs
      Nothing -> True

prop3 :: [OpenTheory.Char.Unicode] -> Bool
prop3 cs = length cs <= length (OpenTheory.Char.encode cs)

main :: IO ()
main =
    do OpenTheory.Test.check "print then parse" prop1
       OpenTheory.Test.check "parse then print" prop2
       OpenTheory.Test.check "printing grows length" prop3
