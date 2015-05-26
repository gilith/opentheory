{- |
module: Prime
description: Generating random primes
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Prime
where

import OpenTheory.Primitive.Natural
import OpenTheory.Primitive.Random as Random
import OpenTheory.Natural
import qualified OpenTheory.Natural.Bits as Bits
import qualified OpenTheory.Natural.Uniform as Uniform

import Random
import qualified Modexp

factorTwos :: Natural -> (Int,Natural)
factorTwos n =
   if Bits.headBits n then (0,n) else (r + 1, s)
  where
    (r,s) = factorTwos (Bits.tailBits n)

millerRabinWitness :: Natural -> Natural -> Bool
millerRabinWitness n =
    \a -> witness (Modexp.modexp n a s) r
  where
    witness x i =
        if i == 0 then x /= 1
        else if x2 == 1 then not (x == 1 || x == n1)
        else witness x2 (i - 1)
      where
        x2 = Modexp.modsquare n x

    (r,s) = factorTwos n1

    n1 = n - 1

millerRabin :: Int -> Natural -> Random.Random -> Bool
millerRabin t n =
    \r -> n == 2 || (n /= 1 && naturalOdd n && trials t r)
  where
    trials i r =
        i == 0 || (trial r1 && trials (i - 1) r2)
      where
        (r1,r2) = Random.split r

    trial = not . millerRabinWitness n . range

    range r = Uniform.random (n - 3) r + 2

isPrime :: Natural -> Random.Random -> Bool
isPrime = millerRabin 100

previousPrime :: Natural -> Random.Random -> Natural
previousPrime n r =
    if isPrime n r1 then n else previousPrime (n - 2) r2
  where
    (r1,r2) = Random.split r

randomPrime :: Int -> Random.Random -> Natural
randomPrime w =
    loop
  where
    loop r =
        case oddPrime r1 of
          Nothing -> loop r2
          Just n -> n
      where
        (r1,r2) = Random.split r

    oddPrime r =
        if isPrime n r2 then Just n else Nothing
      where
        n = randomOdd w r1

        (r1,r2) = Random.split r
