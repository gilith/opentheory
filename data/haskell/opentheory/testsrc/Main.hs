{- |
module: Main
description: The standard theory library - testing
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified Data.Maybe as Maybe
import OpenTheory.Primitive.Test

proposition0 :: Maybe Bool -> Bool
proposition0 x = not (Maybe.isJust x == Maybe.isNothing x)

main :: IO ()
main =
    do check "Proposition 0:\n  !x. ~(isSome x <=> isNone x)\n  " proposition0
       return ()
