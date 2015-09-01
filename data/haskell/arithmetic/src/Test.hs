{- |
module: Main
description: Testing the natural number arithmetic library
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
import qualified Arithmetic.Lucas as Lucas
import qualified Arithmetic.Modular as Modular
import qualified Arithmetic.Montgomery as Montgomery
import qualified Arithmetic.Quadratic as Quadratic
import qualified Arithmetic.Smooth as Smooth

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

propRootFloor :: Natural -> Bool
propRootFloor n =
    sq s <= n && n < sq (s + 1)
  where
    s = Quadratic.rootFloor n
    sq i = i * i

propRootCeiling :: Natural -> Bool
propRootCeiling n =
    (s == 0 || sq (s - 1) < n) && n <= sq s
  where
    s = Quadratic.rootCeiling n
    sq i = i * i

propRootContinuedFraction :: Natural -> Bool
propRootContinuedFraction n =
    cf == spec
  where
    cf = ContinuedFraction.toDouble (Quadratic.rootContinuedFraction n)
    spec = sqrt (fromIntegral n)

propJacobiSymbol :: Natural -> Natural -> Random.Random -> Bool
propJacobiSymbol np m rnd =
    case Quadratic.jacobiSymbol n m of
      Quadratic.Zero -> not coprime
      Quadratic.Residue -> coprime && (mr || not (isPrime n rnd))
      Quadratic.NonResidue -> coprime && not mr
  where
    coprime = gcd m n == 1
    n = 2 * np + 1
    mn = Modular.normalize n m
    mr = any (\k -> Modular.square n k == mn) [1..np]

propNthWilliamsSequence :: Natural -> Natural -> Natural -> Bool
propNthWilliamsSequence np p k =
    Lucas.williamsSequence one two sub mult p !! (fromIntegral k) ==
    Lucas.nthWilliamsSequence two sub mult p k
  where
    n = np + 1
    one = 1
    two = 2
    sub = Modular.subtract n
    mult = Modular.multiply n

propNthWilliamsSequenceProduct ::
    Natural -> Natural -> Natural -> Natural -> Bool
propNthWilliamsSequenceProduct np pp i j =
    Lucas.nthWilliamsSequence two sub mult p (i * j) ==
    Lucas.nthWilliamsSequence two sub mult
      (Lucas.nthWilliamsSequence two sub mult p i) j
  where
    n = np + 1
    p = pp + 1
    two = Modular.normalize n 2
    sub = Modular.subtract n
    mult = Modular.multiply n

propChineseRemainder :: Natural -> Random.Random -> Bool
propChineseRemainder w rnd =
    n `mod` a == x && n `mod` b == y && n < a * b
  where
    (a,b) = randomCoprime w r1
    x = Uniform.random a r2
    y = Uniform.random b r3
    n = chineseRemainder a b x y
    (r1,r23) = Random.split rnd
    (r2,r3) = Random.split r23

propModularNegate :: Natural -> Random.Random -> Bool
propModularNegate nw rnd =
    Modular.add n a b == 0 &&
    b < n
  where
    n = randomWidth nw r1
    a = Uniform.random n r2
    b = Modular.negate n a
    (r1,r2) = Random.split rnd

propModularInvert :: Natural -> Random.Random -> Bool
propModularInvert nw rnd =
    case Modular.invert n a of
      Nothing -> True
      Just b -> Modular.multiply n a b == 1 && b < n
  where
    n = randomWidth nw r1
    a = Uniform.random n r2
    (r1,r2) = Random.split rnd

randomMontgomeryParameters :: Natural -> Random.Random -> Montgomery.Parameters
randomMontgomeryParameters w rnd =
    Montgomery.standardParameters (randomOdd w rnd)

propMontgomeryInvariant :: Natural -> Random.Random -> Bool
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

propMontgomeryNormalize :: Natural -> Random.Random -> Bool
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

propMontgomeryReduce :: Natural -> Random.Random -> Bool
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

propMontgomeryReduceSmall :: Natural -> Random.Random -> Bool
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

propMontgomeryToNatural :: Natural -> Random.Random -> Bool
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

propMontgomeryFromNatural :: Natural -> Random.Random -> Bool
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

propMontgomeryZero :: Natural -> Random.Random -> Bool
propMontgomeryZero nw rnd =
    Montgomery.toNatural (Montgomery.zero p) == 0
  where
    p = randomMontgomeryParameters nw rnd

propMontgomeryOne :: Natural -> Random.Random -> Bool
propMontgomeryOne nw rnd =
    Montgomery.toNatural (Montgomery.one p) == 1
  where
    p = randomMontgomeryParameters nw rnd

propMontgomeryTwo :: Natural -> Random.Random -> Bool
propMontgomeryTwo nw rnd =
    Montgomery.toNatural (Montgomery.two p) == 2
  where
    p = randomMontgomeryParameters nw rnd

propMontgomeryAdd :: Natural -> Random.Random -> Bool
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

propMontgomeryNegate :: Natural -> Random.Random -> Bool
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

propMontgomeryMultiply :: Natural -> Random.Random -> Bool
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

propMontgomeryModexp :: Natural -> Random.Random -> Bool
propMontgomeryModexp w rnd =
    Montgomery.modexp n x k == Modular.exp n x k
  where
    n = randomOdd w r1
    x = Uniform.random n r2
    k = Uniform.random n r3
    (r1,r23) = Random.split rnd
    (r2,r3) = Random.split r23

propMontgomeryModexp2 :: Natural -> Random.Random -> Bool
propMontgomeryModexp2 w rnd =
    Montgomery.modexp2 n x k == Modular.exp2 n x k
  where
    n = randomOdd w r1
    x = Uniform.random n r2
    k = Uniform.random w r3
    (r1,r23) = Random.split rnd
    (r2,r3) = Random.split r23

propFermat :: Natural -> Random.Random -> Bool
propFermat w rnd =
    Montgomery.modexp p a p == a
  where
    p = randomPrime w r1
    a = Uniform.random p r2
    (r1,r2) = Random.split rnd

propRootModuloPrime3Mod4 :: Natural -> Random.Random -> Bool
propRootModuloPrime3Mod4 w rnd =
    Modular.square p r == a
  where
    p = randomPrime3Mod4 w r1
    a = randomFilter (Quadratic.isResidue p) (Uniform.random p) r2
    r = Quadratic.rootModuloPrime3Mod4 p a
    (r1,r2) = Random.split rnd

propRootModuloPrime5Mod8 :: Natural -> Random.Random -> Bool
propRootModuloPrime5Mod8 w rnd =
    Modular.square p r == a
  where
    p = randomPrime5Mod8 w r1
    a = randomFilter (Quadratic.isResidue p) (Uniform.random p) r2
    r = Quadratic.rootModuloPrime5Mod8 p a
    (r1,r2) = Random.split rnd

propRootModuloPrime :: Natural -> Random.Random -> Bool
propRootModuloPrime w rnd =
    Modular.square p r == a
  where
    p = randomPrime w r1
    a = randomFilter (Quadratic.isResidue p) (Uniform.random p) r2
    r = Quadratic.rootModuloPrime p a
    (r1,r2) = Random.split rnd

checkProp :: QuickCheck.Testable prop => String -> prop -> IO ()
checkProp s p = check (s ++ "\n  ") p

checkWidthProp ::
    QuickCheck.Testable prop => Natural -> String -> (Natural -> prop) -> IO ()
checkWidthProp w s p =
    checkProp (s ++ " (" ++ show w ++ " bit)") (p w)

checkWidthProps :: Natural -> IO ()
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
      checkWidthProp w "Square root modulo prime congruent to 3 mod 4" propRootModuloPrime3Mod4
      checkWidthProp w "Square root modulo prime congruent to 5 mod 8" propRootModuloPrime5Mod8
      checkWidthProp w "Square root modulo prime" propRootModuloPrime
      return ()

main :: IO ()
main =
    do checkProp "Result of egcd divides arguments" propEgcdDivides
       checkProp "Result of egcd satisfies equation" propEgcdEquation
       checkProp "Result of egcd below bound" propEgcdBound
       checkProp "Smooth constructor is injective" propSmoothInjective
       checkProp "Floor square root" propRootFloor
       checkProp "Ceiling square root" propRootCeiling
       checkProp "Continued fraction square root" propRootContinuedFraction
       checkProp "Jacobi symbol" propJacobiSymbol
       checkProp "Williams sequence index" propNthWilliamsSequence
       checkProp "Williams sequence product index" propNthWilliamsSequenceProduct
       mapM_ checkWidthProps ws
       return ()
  where
    ws = takeWhile (\n -> n <= 128) (iterate ((*) 2) 4)
