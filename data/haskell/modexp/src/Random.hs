{- |
module: Random
description: Generating random natural numbers of a given width
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Random
where

import Data.Bits
import OpenTheory.Primitive.Natural
import OpenTheory.Primitive.Random as Random
import qualified OpenTheory.Natural.Bits as Bits
import qualified OpenTheory.Natural.Uniform as Uniform

import Egcd

randomWidth :: Int -> Random.Random -> Natural
randomWidth w r =
    n + Uniform.random n r
  where
    n = shiftL 1 (w - 1)

randomOdd :: Int -> Random.Random -> Natural
randomOdd w r = Bits.cons True (randomWidth (w - 1) r)

randomCoprime :: Int -> Random.Random -> (Natural,Natural)
randomCoprime w =
    loop
  where
    loop r =
        case gen r1 of
          Just ab -> ab
          Nothing -> loop r2
      where
        (r1,r2) = Random.split r

    gen r =
        if g == 1 then Just (a,b) else Nothing
      where
        a = randomWidth w r1
        b = randomWidth w r2
        (g,_,_) = naturalEgcd a b
        (r1,r2) = Random.split r

uniformInteger :: Integer -> Random.Random -> Integer
uniformInteger n r = fromIntegral (Uniform.random (fromIntegral n) r)

randomCoprimeInteger :: Int -> Random.Random -> (Integer,Integer)
randomCoprimeInteger w r =
    (fromIntegral a, fromIntegral b)
  where
    (a,b) = randomCoprime w r
