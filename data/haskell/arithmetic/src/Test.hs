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
import OpenTheory.Natural.Divides
import qualified OpenTheory.Primitive.Random as Random
import qualified OpenTheory.Natural.Uniform as Uniform
import OpenTheory.Primitive.Test

import Arithmetic.Random
import Arithmetic.Prime
import qualified Arithmetic.ContinuedFraction as ContinuedFraction
import qualified Arithmetic.Modular as Modular
import qualified Arithmetic.Montgomery as Montgomery
import qualified Arithmetic.Smooth as Smooth
import qualified Arithmetic.SquareRoot as SquareRoot

propEgcdDivides :: Natural -> Natural -> Bool
propEgcdDivides a b =
    divides g a && divides g b
  where
    (g,_) = egcd a b

propEgcdEquation :: Natural -> Natural -> Bool
propEgcdEquation ap b =
    s * a == t * b + g
  where
    a = ap + 1
    (g,(s,t)) = egcd a b

propEgcdBound :: Natural -> Natural -> Bool
propEgcdBound ap b =
    s < max b 2 && t < a
  where
    a = ap + 1
    (_,(s,t)) = egcd a b

propSmoothInjective :: Natural -> Natural -> Bool
propSmoothInjective k np =
    Smooth.toNatural (Smooth.fromNatural k n) == n
  where
    n = np + 1

propFloorSqrt :: Natural -> Bool
propFloorSqrt n =
    sq s <= n && n < sq (s + 1)
  where
    s = SquareRoot.floor n
    sq i = i * i

propCeilingSqrt :: Natural -> Bool
propCeilingSqrt n =
    (s == 0 || sq (s - 1) < n) && n <= sq s
  where
    s = SquareRoot.ceiling n
    sq i = i * i

propContinuedFractionSqrt :: Natural -> Bool
propContinuedFractionSqrt n =
    cf == spec
  where
    cf = ContinuedFraction.toDouble (SquareRoot.continuedFraction n)
    spec = sqrt (fromIntegral n)

propChineseRemainder :: Int -> Random.Random -> Bool
propChineseRemainder w r =
    n `mod` a == x && n `mod` b == y && n < a * b
  where
    (a,b) = randomCoprime w r1
    x = Uniform.random a r2
    y = Uniform.random b r3
    n = chineseRemainder a b x y
    (r1,r23) = Random.split r
    (r2,r3) = Random.split r23

propModularNegate :: Int -> Random.Random -> Bool
propModularNegate nw rnd =
    Modular.add n a b == 0 &&
    b < n
  where
    n = randomWidth nw r1
    a = Uniform.random n r2
    b = Modular.negate n a
    (r1,r2) = Random.split rnd

propModularInvert :: Int -> Random.Random -> Bool
propModularInvert nw rnd =
    case Modular.invert n a of
      Nothing -> True
      Just b -> Modular.multiply n a b == 1 && b < n
  where
    n = randomWidth nw r1
    a = Uniform.random n r2
    (r1,r2) = Random.split rnd

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
      Modular.add n (Montgomery.toNatural a) (Montgomery.toNatural b) &&
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
    Montgomery.toNatural b == Modular.negate n (Montgomery.toNatural a) &&
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
      Modular.multiply n (Montgomery.toNatural a) (Montgomery.toNatural b) &&
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
    Montgomery.modexp n x k == Modular.exp n x k
  where
    n = randomOdd w r1
    x = Uniform.random n r2
    k = Uniform.random n r3

    (r1,r23) = Random.split r
    (r2,r3) = Random.split r23

propMontgomeryModexp2 :: Int -> Random.Random -> Bool
propMontgomeryModexp2 w r =
    Montgomery.modexp2 n x k == Modular.exp2 n x k
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
   do checkWidthProp w "Chinese remainder" propChineseRemainder
      checkWidthProp w "Modular negate" propModularNegate
      checkWidthProp w "Modular invert" propModularInvert
      checkWidthProp w "Montgomery invariant" propMontgomeryInvariant
      checkWidthProp w "Montgomery normalize" propMontgomeryNormalize
      checkWidthProp w "Montgomery reduce" propMontgomeryReduce
      checkWidthProp w "Montgomery reduce small" propMontgomeryReduceSmall
      checkWidthProp w "Montgomery toNatural" propMontgomeryToNatural
      checkWidthProp w "Montgomery fromNatural" propMontgomeryFromNatural
      checkWidthProp w "Montgomery zero" propMontgomeryZero
      checkWidthProp w "Montgomery one" propMontgomeryOne
      checkWidthProp w "Montgomery two" propMontgomeryTwo
      checkWidthProp w "Montgomery add" propMontgomeryAdd
      checkWidthProp w "Montgomery negate" propMontgomeryNegate
      checkWidthProp w "Montgomery multiply" propMontgomeryMultiply
      checkWidthProp w "Montgomery modexp" propMontgomeryModexp
      checkWidthProp w "Montgomery modexp2" propMontgomeryModexp2
      checkWidthProp w "Fermat's little theorem" propFermat
      return ()

main :: IO ()
main =
    do check "Check egcd divides\n  " propEgcdDivides
       check "Check egcd equation\n  " propEgcdEquation
       check "Check egcd bound\n  " propEgcdBound
       check "Check smooth injective\n  " propSmoothInjective
       check "Check floor square root\n  " propFloorSqrt
       check "Check ceiling square root\n  " propCeilingSqrt
       check "Check continued fraction square root\n  " propContinuedFractionSqrt
       mapM_ checkWidthProps ws
       return ()
  where
    ws = takeWhile (\n -> n <= 128) (iterate ((*) 2) 4)
