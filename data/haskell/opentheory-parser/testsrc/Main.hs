{- |
module: Main
description: Stream parsers - testing
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified OpenTheory.Parser.Stream as Stream
import OpenTheory.Primitive.Test

proposition0 :: [Bool] -> Bool
proposition0 l = Stream.toList (Stream.fromList l) == (l, False)

main :: IO ()
main =
    do check "Proposition 0:\n  !l. toList (fromList l) = (l, F)\n  " proposition0
       return ()
