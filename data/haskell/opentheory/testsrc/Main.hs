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

proposition1 :: Bool -> Bool -> Bool
proposition1 a b = not (Left a == Right b)

proposition2 :: [(Bool, Bool)] -> Bool
proposition2 l = (let (x, y) = unzip l in zip x y) == l

main :: IO ()
main =
    do check "Proposition 0:\n  !x. ~(isSome x <=> isNone x)\n  " proposition0
       check "Proposition 1:\n  !a b. ~(left a = right b)\n  " proposition1
       check "Proposition 2:\n  !l. (let (x, y) <- unzip l in zip x y) = l\n  " proposition2
       return ()
