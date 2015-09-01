{- |
module: Arithmetic.Random
description: Generating random natural numbers of a given width
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Random
where

import OpenTheory.Primitive.Natural
import OpenTheory.Primitive.Random as Random
import qualified OpenTheory.Natural.Bits as Bits
import OpenTheory.Natural.Divides
import qualified OpenTheory.Natural.Uniform as Uniform

randomMaybe :: (Random.Random -> Maybe a) -> (Random.Random -> a)
randomMaybe g =
    loop
  where
    loop r =
        case g r1 of
          Just a -> a
          Nothing -> loop r2
      where
        (r1,r2) = Random.split r

randomPredicate :: (Random.Random -> a) -> (a -> Bool) -> (Random.Random -> a)
randomPredicate g p =
    randomMaybe gp
  where
    gp r =
        if p x then Just x else Nothing
      where
        x = g r

randomWidth :: Natural -> Random.Random -> Natural
randomWidth w r =
    n + Uniform.random n r
  where
    n = shiftLeft 1 (w - 1)

randomOdd :: Natural -> Random.Random -> Natural
randomOdd w r = Bits.cons True (randomWidth (w - 1) r)

randomCoprime :: Natural -> Random.Random -> (Natural,Natural)
randomCoprime w =
    randomMaybe gen
  where
    gen r =
        if g == 1 then Just (a,b) else Nothing
      where
        a = randomWidth w r1
        b = randomWidth w r2
        (g,_) = egcd a b
        (r1,r2) = Random.split r
