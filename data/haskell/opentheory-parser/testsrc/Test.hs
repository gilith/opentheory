{- |
Module: Main
Description: Simple stream parsers - testing
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module Main
  ( main )
where

import qualified OpenTheory.Data.List
import qualified OpenTheory.Data.Option
import qualified OpenTheory.Number.Natural
import qualified OpenTheory.Parser.Stream
import qualified OpenTheory.Test

prop0 :: [OpenTheory.Number.Natural.Natural] -> Bool
prop0 l =
  OpenTheory.Data.Option.equal
    (OpenTheory.Data.List.equal OpenTheory.Number.Natural.equal)
    (OpenTheory.Parser.Stream.toList (OpenTheory.Parser.Stream.fromList l))
    (Just l)

prop1 :: ([a], OpenTheory.Parser.Stream.Stream a) -> Bool
prop1 (l, s) =
  OpenTheory.Number.Natural.equal
    (OpenTheory.Parser.Stream.size (OpenTheory.Parser.Stream.append l s))
    (OpenTheory.Data.List.size l + OpenTheory.Parser.Stream.size s)

main :: IO ()
main =
    do OpenTheory.Test.check "prop0" prop0
       OpenTheory.Test.check "prop1" prop1
       return ()
