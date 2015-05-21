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

import Egcd
import Prime
import qualified Modexp
import qualified Montgomery

propIntegerEgcdDivides :: Integer -> Integer -> Bool
propIntegerEgcdDivides a b =
  let (g,_,_) = integerEgcd a b in
  integerDivides g a && integerDivides g b

propIntegerEgcdEquation :: Integer -> Integer -> Bool
propIntegerEgcdEquation a b =
  let (g,s,t) = integerEgcd a b in
  s * a + t * b == g

propIntegerEgcdBound :: Integer -> Integer -> Bool
propIntegerEgcdBound a b =
  let (_,s,t) = integerEgcd a b in
  abs s <= max ((abs b + 1) `div` 2) 1 &&
  abs t <= max ((abs a + 1) `div` 2) 1

propNaturalEgcdDivides :: Natural -> Natural -> Bool
propNaturalEgcdDivides a b =
  let (g,_,_) = naturalEgcd a b in
  naturalDivides g a && naturalDivides g b

propNaturalEgcdEquation :: Natural -> Natural -> Bool
propNaturalEgcdEquation ap b =
  let a = ap + 1 in
  let (g,s,t) = naturalEgcd a b in
  s * a == t * b + g

propNaturalEgcdBound :: Natural -> Natural -> Bool
propNaturalEgcdBound ap b =
  let a = ap + 1 in
  let (_,s,t) = naturalEgcd a b in
  s < max b 2 && t < a

randomMontgomery :: Int -> Random.Random -> Montgomery.Montgomery
randomMontgomery w r = Montgomery.standard (randomOdd w r)

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
    n = randomOdd w r1
    x = Uniform.random n r2
    k = Uniform.random n r3

    (r1,r23) = Random.split r
    (r2,r3) = Random.split r23

propMontgomeryModexp2 :: Int -> Random.Random -> Bool
propMontgomeryModexp2 w r =
    Montgomery.modexp2 n x k == Modexp.modexp2 n x k
  where
    n = randomOdd w r1
    x = Uniform.random n r2
    k = Uniform.random (fromIntegral w) r3

    (r1,r23) = Random.split r
    (r2,r3) = Random.split r23

checkWidthProp ::
    QuickCheck.Testable prop => Int -> String -> (Int -> prop) -> IO ()
checkWidthProp w s p =
    check (s ++ " (" ++ show w ++ " bit)\n  ") (p w)

checkWidthProps :: Int -> IO ()
checkWidthProps w =
   do checkWidthProp w "Check Montgomery invariant" propMontgomeryInvariant
      checkWidthProp w "Check Montgomery normalize" propMontgomeryNormalize
      checkWidthProp w "Check Montgomery reduce" propMontgomeryReduce
      checkWidthProp w "Check Montgomery reduce small" propMontgomeryReduceSmall
      checkWidthProp w "Check Montgomery toNatural" propMontgomeryToNatural
      checkWidthProp w "Check Montgomery fromNatural" propMontgomeryFromNatural
      checkWidthProp w "Check Montgomery oneM" propMontgomeryOneM
      checkWidthProp w "Check Montgomery addM" propMontgomeryAddM
      checkWidthProp w "Check Montgomery multiplyM" propMontgomeryMultiplyM
      checkWidthProp w "Check Montgomery modexp" propMontgomeryModexp
      checkWidthProp w "Check Montgomery modexp2" propMontgomeryModexp2
      return ()

main :: IO ()
main =
    do check "Check integer egcd divides\n  " propIntegerEgcdDivides
       check "Check integer egcd equation\n  " propIntegerEgcdEquation
       check "Check integer egcd bound\n  " propIntegerEgcdBound
       check "Check natural egcd divides\n  " propNaturalEgcdDivides
       check "Check natural egcd equation\n  " propNaturalEgcdEquation
       check "Check natural egcd bound\n  " propNaturalEgcdBound
       mapM_ checkWidthProps ws
       return ()
  where
    ws = takeWhile (\n -> n <= 256) (iterate ((*) 2) 4)
