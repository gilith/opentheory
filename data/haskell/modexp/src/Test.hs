{- |
module: Main
description: Testing the modular exponentiation computation
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified Test.QuickCheck as QuickCheck
import OpenTheory.Primitive.Natural
import OpenTheory.Natural
import qualified OpenTheory.Primitive.Random as Random
import qualified OpenTheory.Natural.Uniform as Uniform
import OpenTheory.Primitive.Test

import qualified Egcd
import qualified Prime
import qualified Modexp
import qualified Montgomery

widthCheck ::
    QuickCheck.Testable prop => Int -> String -> (Int -> prop) -> IO ()
widthCheck w s p =
    check (s ++ " (" ++ show w ++ " bit)\n  ") (p w)

propEgcdEquation :: Natural -> Natural -> Bool
propEgcdEquation ap b =
  let a = ap + 1 in
  let (g,s,t) = Egcd.naturalEgcd a b in
  s * a == t * b + g

propEgcdBound :: Natural -> Natural -> Bool
propEgcdBound ap b =
  let a = ap + 1 in
  let (_,s,t) = Egcd.naturalEgcd a b in
  (if b < 2 then s == 1 else 0 < s && s < b) && t < a

randomMontgomery :: Int -> Random.Random -> Montgomery.Montgomery
randomMontgomery w r = Montgomery.standard (Prime.randomOdd w r)

propMontgomeryInvariant :: Int -> Random.Random -> Bool
propMontgomeryInvariant nw rnd =
    naturalOdd n &&
    n < w2 &&
    s * w2 == k * n + 1 &&
    s < n &&
    k < w2 &&
    r == w2 `mod` n &&
    r2 == (r * r) `mod` n
  where
    Montgomery.Montgomery
      {Montgomery.nMontgomery = n,
       Montgomery.wMontgomery = w,
       Montgomery.sMontgomery = s,
       Montgomery.kMontgomery = k,
       Montgomery.rMontgomery = r,
       Montgomery.r2Montgomery = r2} = randomMontgomery nw rnd

    w2 = shiftLeft 1 w

propMontgomeryNormalize :: Int -> Random.Random -> Bool
propMontgomeryNormalize nw rnd =
    x `mod` n == a `mod` n &&
    x < w2
  where
    m = randomMontgomery nw r1
    a = Uniform.random (w2 * w2) r2
    x = Montgomery.normalize m a

    n = Montgomery.nMontgomery m
    w = Montgomery.wMontgomery m
    w2 = shiftLeft 1 w
    (r1,r2) = Random.split rnd

propMontgomeryReduce :: Int -> Random.Random -> Bool
propMontgomeryReduce nw rnd =
    x `mod` n == (a * s) `mod` n &&
    x < w2 + n
  where
    m = randomMontgomery nw r1
    a = Uniform.random (w2 * w2) r2
    x = Montgomery.reduce m a

    n = Montgomery.nMontgomery m
    w = Montgomery.wMontgomery m
    s = Montgomery.sMontgomery m
    w2 = shiftLeft 1 w
    (r1,r2) = Random.split rnd

propMontgomeryReduceSmall :: Int -> Random.Random -> Bool
propMontgomeryReduceSmall nw rnd =
    x `mod` n == (a * s) `mod` n &&
    x <= n
  where
    m = randomMontgomery nw r1
    a = Uniform.random w2 r2
    x = Montgomery.reduce m a

    n = Montgomery.nMontgomery m
    w = Montgomery.wMontgomery m
    s = Montgomery.sMontgomery m
    w2 = shiftLeft 1 w
    (r1,r2) = Random.split rnd

propMontgomeryToNatural :: Int -> Random.Random -> Bool
propMontgomeryToNatural nw rnd =
    x == (a * s) `mod` n
  where
    m = randomMontgomery nw r1
    a = Uniform.random w2 r2
    x = Montgomery.toNatural m a

    n = Montgomery.nMontgomery m
    w = Montgomery.wMontgomery m
    s = Montgomery.sMontgomery m
    w2 = shiftLeft 1 w
    (r1,r2) = Random.split rnd

propMontgomeryFromNatural :: Int -> Random.Random -> Bool
propMontgomeryFromNatural nw rnd =
    x == a `mod` n
  where
    m = randomMontgomery nw r1
    a = Uniform.random (w2 * w2) r2
    x = Montgomery.toNatural m (Montgomery.fromNatural m a)

    n = Montgomery.nMontgomery m
    w = Montgomery.wMontgomery m
    w2 = shiftLeft 1 w
    (r1,r2) = Random.split rnd

propMontgomeryOneM :: Int -> Random.Random -> Bool
propMontgomeryOneM nw rnd =
    Montgomery.toNatural m (Montgomery.oneM m) == 1
  where
    m = randomMontgomery nw rnd

propMontgomeryAddM :: Int -> Random.Random -> Bool
propMontgomeryAddM nw rnd =
    Montgomery.toNatural m x ==
      Modexp.modadd n (Montgomery.toNatural m a) (Montgomery.toNatural m b) &&
    x < w2
  where
    m = randomMontgomery nw r1
    a = Uniform.random w2 r2
    b = Uniform.random w2 r3
    x = Montgomery.addM m a b

    n = Montgomery.nMontgomery m
    w = Montgomery.wMontgomery m
    w2 = shiftLeft 1 w
    (r1,r23) = Random.split rnd
    (r2,r3) = Random.split r23

propMontgomeryMultiplyM :: Int -> Random.Random -> Bool
propMontgomeryMultiplyM nw rnd =
    Montgomery.toNatural m x ==
      Modexp.modmult n (Montgomery.toNatural m a) (Montgomery.toNatural m b) &&
    x < w2
  where
    m = randomMontgomery nw r1
    a = Uniform.random w2 r2
    b = Uniform.random w2 r3
    x = Montgomery.multiplyM m a b

    n = Montgomery.nMontgomery m
    w = Montgomery.wMontgomery m
    w2 = shiftLeft 1 w
    (r1,r23) = Random.split rnd
    (r2,r3) = Random.split r23

propMontgomeryModexp :: Int -> Random.Random -> Bool
propMontgomeryModexp w r =
    Montgomery.modexp n x k == Modexp.modexp n x k
  where
    n = Prime.randomOdd w r1
    x = Uniform.random n r2
    k = Uniform.random n r3

    (r1,r23) = Random.split r
    (r2,r3) = Random.split r23

propMontgomeryModexp2 :: Int -> Random.Random -> Bool
propMontgomeryModexp2 w r =
    Montgomery.modexp2 n x k == Modexp.modexp2 n x k
  where
    n = Prime.randomOdd w r1
    x = Uniform.random n r2
    k = Uniform.random (fromIntegral w) r3

    (r1,r23) = Random.split r
    (r2,r3) = Random.split r23

checkWidth :: Int -> IO ()
checkWidth w =
   do widthCheck w "Check Montgomery invariant" propMontgomeryInvariant
      widthCheck w "Check Montgomery normalize" propMontgomeryNormalize
      widthCheck w "Check Montgomery reduce" propMontgomeryReduce
      widthCheck w "Check Montgomery reduce small" propMontgomeryReduceSmall
      widthCheck w "Check Montgomery toNatural" propMontgomeryToNatural
      widthCheck w "Check Montgomery fromNatural" propMontgomeryFromNatural
      widthCheck w "Check Montgomery oneM" propMontgomeryOneM
      widthCheck w "Check Montgomery addM" propMontgomeryAddM
      widthCheck w "Check Montgomery multiplyM" propMontgomeryMultiplyM
      widthCheck w "Check Montgomery modexp" propMontgomeryModexp
      widthCheck w "Check Montgomery modexp2" propMontgomeryModexp2
      return ()

checkWidths :: IO ()
checkWidths =
    mapM_ checkWidth ws
  where
    ws = takeWhile (\n -> n <= 256) (iterate ((*) 2) 4)

main :: IO ()
main =
    do check "Check egcd equation\n  " propEgcdEquation
       check "Check egcd bound\n  " propEgcdBound
       checkWidths
       return ()
