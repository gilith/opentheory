{- |
module: Arithmetic.Prime
description: Generating random primes
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Prime
where

import OpenTheory.Primitive.Natural
import OpenTheory.Primitive.Random as Random
import OpenTheory.Natural
import qualified OpenTheory.Natural.Uniform as Uniform

import Arithmetic.Random
import Arithmetic.Utility
import qualified Arithmetic.Modular as Modular
import qualified Arithmetic.Prime.Sieve as Sieve

primes :: [Natural]
primes = 2 : Sieve.advance 1 4 Sieve.initial

millerRabinWitness :: Natural -> Natural -> Bool
millerRabinWitness n =
    \a -> witness (Modular.exp n a s) r
  where
    witness x i =
        if i == 0 then x /= 1
        else if x2 == 1 then not (x == 1 || x == n1)
        else witness x2 (i - 1)
      where
        x2 = Modular.square n x

    (r,s) = factorTwos n1

    n1 = n - 1

millerRabin :: Natural -> Natural -> Random.Random -> Bool
millerRabin t n =
    \r -> n == 2 || n == 3 || (n /= 1 && naturalOdd n && trials t r)
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
    if isPrime n r1 then n else previousPrime (n - 1) r2
  where
    (r1,r2) = Random.split r

nextPrime :: Natural -> Random.Random -> Natural
nextPrime n r =
    if isPrime n r1 then n else nextPrime (n + 1) r2
  where
    (r1,r2) = Random.split r

nextPrime3Mod4 :: Natural -> Random.Random -> Natural
nextPrime3Mod4 =
    \n -> go ((4 * (n `div` 4)) + 3)
  where
    go n r =
        if isPrime n r1 then n else go (n + 4) r2
      where
        (r1,r2) = Random.split r

nextPrime5Mod8 :: Natural -> Random.Random -> Natural
nextPrime5Mod8 =
    \n -> go ((8 * ((n + 2) `div` 8)) + 5)
  where
    go n r =
        if isPrime n r1 then n else go (n + 8) r2
      where
        (r1,r2) = Random.split r

randomPrime :: Natural -> Random.Random -> Natural
randomPrime w =
    randomMaybe gen
  where
    gen r =
        if isPrime n r2 then Just n else Nothing
      where
        n = randomOdd w r1
        (r1,r2) = Random.split r

randomPrime3Mod4 :: Natural -> Random.Random -> Natural
randomPrime3Mod4 w =
    randomFilter check (randomPrime w)
  where
    check p = p `mod` 4 == 3

randomPrime5Mod8 :: Natural -> Random.Random -> Natural
randomPrime5Mod8 w =
    randomFilter check (randomPrime w)
  where
    check p = p `mod` 8 == 5
