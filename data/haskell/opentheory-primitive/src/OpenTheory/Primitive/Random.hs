{- |
module: $Header$
description: Primitive random bit-stream functions
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Primitive.Random
  ( Random,
    bit,
    split,
    fromInt )
where

import qualified System.Random
import qualified Test.QuickCheck

data Random =
    Random
      { seed :: Int,
        gen :: System.Random.StdGen }

instance Test.QuickCheck.Arbitrary Random where
  arbitrary = fmap fromInt Test.QuickCheck.arbitrary

instance Show Random where
  show r = "Random<" ++ show (seed r) ++ ">"

bit :: Random -> Bool
bit r = fst (System.Random.random (gen r))

split :: Random -> (Random,Random)
split r =
  let (g1,g2) = System.Random.split (gen r) in
  (r {gen = g1}, r {gen = g2})

fromInt :: Int -> Random
fromInt i =
    Random
      {seed = i,
       gen = System.Random.mkStdGen i}
