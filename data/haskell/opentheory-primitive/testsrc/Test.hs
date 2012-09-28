{- |
Module: $Header$
Description: OpenTheory QuickCheck interface
License: MIT

Maintainer: Joe Leslie-Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

OpenTheory QuickCheck interface
-}
module Main
  ( main )
where

import qualified OpenTheory.Primitive.Random as Primitive.Random
import qualified OpenTheory.Primitive.Test as Primitive.Test

proposition0 :: Primitive.Random.Random -> Bool
proposition0 r =
  let (p,_) = Primitive.Random.bit r in
  p || not p

main :: IO ()
main =
    do Primitive.Test.check "Proposition 0:\n  !p. p \\/ ~p\n  " proposition0
       return ()
