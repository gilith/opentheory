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

import qualified IntegerDivides
import qualified NaturalDivides
import Arithmetic.Random
import Arithmetic.Prime
import qualified Arithmetic.Modular as Modular
import qualified Arithmetic.Montgomery as Montgomery

propIntegerEgcdDivides :: Integer -> Integer -> Bool
propIntegerEgcdDivides a b =
    let (g,_) = IntegerDivides.egcd a b in
    IntegerDivides.divides g a && IntegerDivides.divides g b

propIntegerEgcdEquation :: Integer -> Integer -> Bool
propIntegerEgcdEquation a b =
    let (g,(s,t)) = IntegerDivides.egcd a b in
    s * a + t * b == g

propIntegerEgcdBound :: Integer -> Integer -> Bool
propIntegerEgcdBound a b =
    let (_,(s,t)) = IntegerDivides.egcd a b in
    abs s <= max ((abs b + 1) `div` 2) 1 &&
    abs t <= max ((abs a + 1) `div` 2) 1

propNaturalEgcdDivides :: Natural -> Natural -> Bool
propNaturalEgcdDivides a b =
    let (g,_) = NaturalDivides.egcd a b in
    NaturalDivides.divides g a && NaturalDivides.divides g b

propNaturalEgcdEquation :: Natural -> Natural -> Bool
propNaturalEgcdEquation ap b =
    let a = ap + 1 in
    let (g,(s,t)) = NaturalDivides.egcd a b in
    s * a == t * b + g

propNaturalEgcdBound :: Natural -> Natural -> Bool
propNaturalEgcdBound ap b =
    let a = ap + 1 in
    let (_,(s,t)) = NaturalDivides.egcd a b in
    s < max b 2 && t < a

propIntegerChineseRemainder :: Int -> Random.Random -> Bool
propIntegerChineseRemainder w r =
    n `mod` a == x && n `mod` b == y && n < a * b
  where
    (a,b) = randomCoprimeInteger w r1
    x = uniformInteger a r2
    y = uniformInteger b r3
    n = IntegerDivides.chineseRemainder a b x y
    (r1,r23) = Random.split r
    (r2,r3) = Random.split r23

propNaturalChineseRemainder :: Int -> Random.Random -> Bool
propNaturalChineseRemainder w r =
    n `mod` a == x && n `mod` b == y && n < a * b
  where
    (a,b) = randomCoprime w r1
    x = Uniform.random a r2
    y = Uniform.random b r3
    n = NaturalDivides.chineseRemainder a b x y
    (r1,r23) = Random.split r
    (r2,r3) = Random.split r23

randomMontgomeryParameters :: Int -> Random.Random -> Montgomery.Parameters
randomMontgomeryParameters w r = Montgomery.standardParameters (randomOdd w r)

propMontgomeryInvariant :: Int -> Random.Random -> Bool
propMontgomeryInvariant nw rnd =
    naturalOdd n &&
    n < w2 &&
    s * w2 == k * n + 1 &&
    s < n &&
    k < w2 &&
    r == w2 `mod` n &&
    r2 == (r * r) `mod` n &&
    z `mod` n == 0 &&
    w2 <= z &&
    z < w2 + n
  where
    Montgomery.Parameters
      {Montgomery.nParameters = n,
       Montgomery.wParameters = w,
       Montgomery.sParameters = s,
       Montgomery.kParameters = k,
       Montgomery.rParameters = r,
       Montgomery.r2Parameters = r2,
       Montgomery.zParameters = z} = randomMontgomeryParameters nw rnd

    w2 = shiftLeft 1 w

propMontgomeryNormalize :: Int -> Random.Random -> Bool
propMontgomeryNormalize nw rnd =
    b `mod` n == a `mod` n &&
    b < w2
  where
    p = randomMontgomeryParameters nw r1
    a = Uniform.random (w2 * w2) r2
    b = Montgomery.nMontgomery (Montgomery.normalize p a)

    n = Montgomery.nParameters p
    w = Montgomery.wParameters p
    w2 = shiftLeft 1 w
    (r1,r2) = Random.split rnd

propMontgomeryReduce :: Int -> Random.Random -> Bool
propMontgomeryReduce nw rnd =
    b `mod` n == (a * s) `mod` n &&
    b < w2 + n
  where
    p = randomMontgomeryParameters nw r1
    a = Uniform.random (w2 * w2) r2
    b = Montgomery.reduce p a

    n = Montgomery.nParameters p
    w = Montgomery.wParameters p
    s = Montgomery.sParameters p
    w2 = shiftLeft 1 w
    (r1,r2) = Random.split rnd

propMontgomeryReduceSmall :: Int -> Random.Random -> Bool
propMontgomeryReduceSmall nw rnd =
    b `mod` n == (a * s) `mod` n &&
    b <= n
  where
    p = randomMontgomeryParameters nw r1
    a = Uniform.random w2 r2
    b = Montgomery.reduce p a

    n = Montgomery.nParameters p
    w = Montgomery.wParameters p
    s = Montgomery.sParameters p
    w2 = shiftLeft 1 w
    (r1,r2) = Random.split rnd

propMontgomeryToNatural :: Int -> Random.Random -> Bool
propMontgomeryToNatural nw rnd =
    b == (a * s) `mod` n
  where
    p = randomMontgomeryParameters nw r1
    a = Uniform.random w2 r2
    b = Montgomery.toNatural (Montgomery.normalize p a)

    n = Montgomery.nParameters p
    w = Montgomery.wParameters p
    s = Montgomery.sParameters p
    w2 = shiftLeft 1 w
    (r1,r2) = Random.split rnd

propMontgomeryFromNatural :: Int -> Random.Random -> Bool
propMontgomeryFromNatural nw rnd =
    b == a `mod` n
  where
    p = randomMontgomeryParameters nw r1
    a = Uniform.random (w2 * w2) r2
    b = Montgomery.toNatural (Montgomery.fromNatural p a)

    n = Montgomery.nParameters p
    w = Montgomery.wParameters p
    w2 = shiftLeft 1 w
    (r1,r2) = Random.split rnd

propMontgomeryZero :: Int -> Random.Random -> Bool
propMontgomeryZero nw rnd =
    Montgomery.toNatural (Montgomery.zero p) == 0
  where
    p = randomMontgomeryParameters nw rnd

propMontgomeryOne :: Int -> Random.Random -> Bool
propMontgomeryOne nw rnd =
    Montgomery.toNatural (Montgomery.one p) == 1
  where
    p = randomMontgomeryParameters nw rnd

propMontgomeryTwo :: Int -> Random.Random -> Bool
propMontgomeryTwo nw rnd =
    Montgomery.toNatural (Montgomery.two p) == 2
  where
    p = randomMontgomeryParameters nw rnd

propMontgomeryAdd :: Int -> Random.Random -> Bool
propMontgomeryAdd nw rnd =
    Montgomery.toNatural c ==
      Modular.modadd n (Montgomery.toNatural a) (Montgomery.toNatural b) &&
    Montgomery.nMontgomery c < w2
  where
    p = randomMontgomeryParameters nw r1
    a = Montgomery.normalize p (Uniform.random w2 r2)
    b = Montgomery.normalize p (Uniform.random w2 r3)
    c = Montgomery.add a b

    n = Montgomery.nParameters p
    w = Montgomery.wParameters p
    w2 = shiftLeft 1 w
    (r1,r23) = Random.split rnd
    (r2,r3) = Random.split r23

propMontgomeryNegate :: Int -> Random.Random -> Bool
propMontgomeryNegate nw rnd =
    Montgomery.toNatural b == Modular.modneg n (Montgomery.toNatural a) &&
    Montgomery.nMontgomery b < w2
  where
    p = randomMontgomeryParameters nw r1
    a = Montgomery.normalize p (Uniform.random w2 r2)
    b = Montgomery.negate a

    n = Montgomery.nParameters p
    w = Montgomery.wParameters p
    w2 = shiftLeft 1 w
    (r1,r2) = Random.split rnd

propMontgomeryMultiply :: Int -> Random.Random -> Bool
propMontgomeryMultiply nw rnd =
    Montgomery.toNatural c ==
      Modular.modmult n (Montgomery.toNatural a) (Montgomery.toNatural b) &&
    Montgomery.nMontgomery c < w2
  where
    p = randomMontgomeryParameters nw r1
    a = Montgomery.normalize p (Uniform.random w2 r2)
    b = Montgomery.normalize p (Uniform.random w2 r3)
    c = Montgomery.multiply a b

    n = Montgomery.nParameters p
    w = Montgomery.wParameters p
    w2 = shiftLeft 1 w
    (r1,r23) = Random.split rnd
    (r2,r3) = Random.split r23

propMontgomeryModexp :: Int -> Random.Random -> Bool
propMontgomeryModexp w r =
    Montgomery.modexp n x k == Modular.modexp n x k
  where
    n = randomOdd w r1
    x = Uniform.random n r2
    k = Uniform.random n r3

    (r1,r23) = Random.split r
    (r2,r3) = Random.split r23

propMontgomeryModexp2 :: Int -> Random.Random -> Bool
propMontgomeryModexp2 w r =
    Montgomery.modexp2 n x k == Modular.modexp2 n x k
  where
    n = randomOdd w r1
    x = Uniform.random n r2
    k = Uniform.random (fromIntegral w) r3

    (r1,r23) = Random.split r
    (r2,r3) = Random.split r23

propFermat :: Int -> Random.Random -> Bool
propFermat w r =
    Montgomery.modexp n a n == a
  where
    n = randomPrime w r1
    a = Uniform.random n r2
    (r1,r2) = Random.split r

checkWidthProp ::
    QuickCheck.Testable prop => Int -> String -> (Int -> prop) -> IO ()
checkWidthProp w s p =
    check (s ++ " (" ++ show w ++ " bit)\n  ") (p w)

checkWidthProps :: Int -> IO ()
checkWidthProps w =
   do checkWidthProp w "Check integer Chinese remainder properties"
        propIntegerChineseRemainder
      checkWidthProp w "Check natural Chinese remainder properties"
        propNaturalChineseRemainder
      checkWidthProp w "Check Montgomery invariant" propMontgomeryInvariant
      checkWidthProp w "Check Montgomery normalize" propMontgomeryNormalize
      checkWidthProp w "Check Montgomery reduce" propMontgomeryReduce
      checkWidthProp w "Check Montgomery reduce small" propMontgomeryReduceSmall
      checkWidthProp w "Check Montgomery toNatural" propMontgomeryToNatural
      checkWidthProp w "Check Montgomery fromNatural" propMontgomeryFromNatural
      checkWidthProp w "Check Montgomery zero" propMontgomeryZero
      checkWidthProp w "Check Montgomery one" propMontgomeryOne
      checkWidthProp w "Check Montgomery two" propMontgomeryTwo
      checkWidthProp w "Check Montgomery add" propMontgomeryAdd
      checkWidthProp w "Check Montgomery negate" propMontgomeryNegate
      checkWidthProp w "Check Montgomery multiply" propMontgomeryMultiply
      checkWidthProp w "Check Montgomery modexp" propMontgomeryModexp
      checkWidthProp w "Check Montgomery modexp2" propMontgomeryModexp2
      checkWidthProp w "Fermat's little theorem" propFermat
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
    ws = takeWhile (\n -> n <= 128) (iterate ((*) 2) 4)
