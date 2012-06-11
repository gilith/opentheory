{- |
Module: $Header$
Description: Random stream primitives
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

Random stream primitives
-}
module OpenTheory.Primitive.Probability.Random
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

bit :: Random -> (Bool,Random)
bit r =
  let (b,g) = System.Random.random (gen r) in
  (b, r {gen = g})

split :: Random -> (Random,Random)
split r =
  let (g1,g2) = System.Random.split (gen r) in
  (r {gen = g1}, r {gen = g2})

fromInt :: Int -> Random
fromInt i =
    Random
      {seed = i,
       gen = System.Random.mkStdGen i}
