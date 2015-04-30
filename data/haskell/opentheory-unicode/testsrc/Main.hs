{- |
module: Main
description: Unicode characters - testing
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified OpenTheory.List as List
import qualified OpenTheory.Primitive.Byte as Byte
import qualified OpenTheory.Unicode as Unicode
import qualified OpenTheory.Unicode.UTF8 as UTF8
import OpenTheory.Primitive.Test

proposition0 :: [Byte.Byte] -> Bool
proposition0 b = UTF8.reencode (UTF8.decode b) == b

proposition1 :: [Byte.Byte] -> Bool
proposition1 b = List.naturalLength (UTF8.decode b) <= List.naturalLength b

proposition2 :: [Unicode.Unicode] -> Bool
proposition2 c = List.naturalLength c <= List.naturalLength (UTF8.encode c)

proposition3 :: [Either Byte.Byte Unicode.Unicode] -> Bool
proposition3 c =
  List.naturalLength c <= List.naturalLength (UTF8.reencode c)

proposition4 :: [Unicode.Unicode] -> Bool
proposition4 c = UTF8.decode (UTF8.encode c) == map Right c

main :: IO ()
main =
    do check "Proposition 0:\n  !b. UTF8.reencode (UTF8.decode b) = b\n  " proposition0
       check "Proposition 1:\n  !b. length (UTF8.decode b) <= length b\n  " proposition1
       check "Proposition 2:\n  !c. length c <= length (UTF8.encode c)\n  " proposition2
       check "Proposition 3:\n  !c. length c <= length (UTF8.reencode c)\n  " proposition3
       check "Proposition 4:\n  !c. UTF8.decode (UTF8.encode c) = map right c\n  " proposition4
       return ()
