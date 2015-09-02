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

propSmoothInjective :: Natural -> Natural -> Bool
propSmoothInjective k np =
    Smooth.toNatural (Smooth.fromNatural k n) == n
  where
    n = np + 1

propModularNegate :: Natural -> Random.Random -> Bool
propModularNegate np rnd =
    Modular.add n a b == 0 &&
    b < n
  where
    n = np + 1
    a = Uniform.random n rnd
    b = Modular.negate n a

propModularInvert :: Natural -> Natural -> Bool
propModularInvert np a =
    case Modular.invert n a of
      Nothing -> gcd n a /= 1
      Just b -> Modular.multiply n a b == Modular.normalize n 1 && b < n
  where
    n = np + 1

propFermat :: Natural -> Random.Random -> Bool
propFermat pp rnd =
    Modular.exp p a p == a
  where
    p = nextPrime (pp + 3) r1
    a = Uniform.random p r2
    (r1,r2) = Random.split rnd

propMontgomeryInvariant :: Natural -> Bool
propMontgomeryInvariant np =
    naturalOdd n &&
    1 < n &&
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
       Montgomery.zParameters = z} = Montgomery.standardParameters (2 * np + 3)
    w2 = shiftLeft 1 w

propMontgomeryNormalize :: Natural -> Natural -> Bool
propMontgomeryNormalize np a =
    b `mod` n == a `mod` n &&
    b < w2
  where
    n = 2 * np + 3
    p = Montgomery.standardParameters n
    b = Montgomery.nMontgomery (Montgomery.normalize p a)
    w = Montgomery.wParameters p
    w2 = shiftLeft 1 w

propMontgomeryReduce :: Natural -> Natural -> Bool
propMontgomeryReduce np a =
    b `mod` n == (a * s) `mod` n &&
    b < w2 + n
  where
    n = 2 * np + 3
    p = Montgomery.standardParameters n
    b = Montgomery.reduce p a
    w = Montgomery.wParameters p
    s = Montgomery.sParameters p
    w2 = shiftLeft 1 w

propMontgomeryReduceSmall :: Natural -> Natural -> Bool
propMontgomeryReduceSmall np ap =
    b `mod` n == (a * s) `mod` n &&
    b <= n
  where
    n = 2 * np + 3
    p = Montgomery.standardParameters n
    a = ap `mod` w2
    b = Montgomery.reduce p a
    w = Montgomery.wParameters p
    s = Montgomery.sParameters p
    w2 = shiftLeft 1 w

propMontgomeryToNatural :: Natural -> Natural -> Bool
propMontgomeryToNatural np a =
    b == (a * s) `mod` n
  where
    n = 2 * np + 3
    p = Montgomery.standardParameters n
    b = Montgomery.toNatural (Montgomery.normalize p a)
    s = Montgomery.sParameters p

propMontgomeryFromNatural :: Natural -> Natural -> Bool
propMontgomeryFromNatural np a =
    b == a `mod` n
  where
    n = 2 * np + 3
    p = Montgomery.standardParameters n
    b = Montgomery.toNatural (Montgomery.fromNatural p a)

propMontgomeryZero :: Natural -> Bool
propMontgomeryZero np =
    Montgomery.toNatural (Montgomery.zero p) == 0
  where
    n = 2 * np + 3
    p = Montgomery.standardParameters n

propMontgomeryOne :: Natural -> Bool
propMontgomeryOne np =
    Montgomery.toNatural (Montgomery.one p) == 1
  where
    n = 2 * np + 3
    p = Montgomery.standardParameters n

propMontgomeryTwo :: Natural -> Bool
propMontgomeryTwo np =
    Montgomery.toNatural (Montgomery.two p) == 2
  where
    n = 2 * np + 3
    p = Montgomery.standardParameters n

propMontgomeryAdd :: Natural -> Natural -> Natural -> Bool
propMontgomeryAdd np ap bp =
    Montgomery.toNatural c ==
      Modular.add n (Montgomery.toNatural a) (Montgomery.toNatural b) &&
    Montgomery.nMontgomery c < w2
  where
    n = 2 * np + 3
    p = Montgomery.standardParameters n
    a = Montgomery.normalize p ap
    b = Montgomery.normalize p bp
    c = Montgomery.add a b
    w = Montgomery.wParameters p
    w2 = shiftLeft 1 w

propMontgomeryNegate :: Natural -> Natural -> Bool
propMontgomeryNegate np ap =
    Montgomery.toNatural b == Modular.negate n (Montgomery.toNatural a) &&
    Montgomery.nMontgomery b < w2
  where
    n = 2 * np + 3
    p = Montgomery.standardParameters n
    a = Montgomery.normalize p ap
    b = Montgomery.negate a
    w = Montgomery.wParameters p
    w2 = shiftLeft 1 w

propMontgomeryMultiply :: Natural -> Natural -> Natural -> Bool
propMontgomeryMultiply np ap bp =
    Montgomery.toNatural c ==
      Modular.multiply n (Montgomery.toNatural a) (Montgomery.toNatural b) &&
    Montgomery.nMontgomery c < w2
  where
    n = 2 * np + 3
    p = Montgomery.standardParameters n
    a = Montgomery.normalize p ap
    b = Montgomery.normalize p bp
    c = Montgomery.multiply a b
    w = Montgomery.wParameters p
    w2 = shiftLeft 1 w

propMontgomeryModexp :: Natural -> Natural -> Natural -> Bool
propMontgomeryModexp np x k =
    Montgomery.modexp n x k == Modular.exp n x k
  where
    n = 2 * np + 3

propMontgomeryModexp2 :: Natural -> Natural -> Natural -> Bool
propMontgomeryModexp2 np x k =
    Montgomery.modexp2 n x k == Modular.exp2 n x k
  where
    n = 2 * np + 3

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

propRootModuloPrime3Mod4 :: Natural -> Random.Random -> Bool
propRootModuloPrime3Mod4 pp rnd =
    Modular.square p r == a
  where
    p = nextPrime3Mod4 pp r1
    a = randomFilter (Quadratic.isResidue p) (Uniform.random p) r2
    r = Quadratic.rootModuloPrime3Mod4 p a
    (r1,r2) = Random.split rnd

propRootModuloPrime5Mod8 :: Natural -> Random.Random -> Bool
propRootModuloPrime5Mod8 pp rnd =
    Modular.square p r == a
  where
    p = nextPrime5Mod8 pp r1
    a = randomFilter (Quadratic.isResidue p) (Uniform.random p) r2
    r = Quadratic.rootModuloPrime5Mod8 p a
    (r1,r2) = Random.split rnd

propRootModuloPrime :: Natural -> Random.Random -> Bool
propRootModuloPrime pp rnd =
    Modular.square p r == a
  where
    p = nextPrime pp r1
    a = randomFilter (Quadratic.isResidue p) (Uniform.random p) r2
    r = Quadratic.rootModuloPrime p a
    (r1,r2) = Random.split rnd

propWilliamsNth :: Natural -> Natural -> Natural -> Bool
propWilliamsNth np p k =
    Lucas.williamsSequence one two sub mult p !! (fromIntegral k) ==
    Lucas.williamsNth two sub mult p k
  where
    n = np + 1
    one = 1
    two = 2
    sub = Modular.subtract n
    mult = Modular.multiply n

propWilliamsNthProduct :: Natural -> Natural -> Natural -> Natural -> Bool
propWilliamsNthProduct np pp i j =
    Lucas.williamsNth two sub mult p (i * j) ==
    Lucas.williamsNth two sub mult
      (Lucas.williamsNth two sub mult p i) j
  where
    n = np + 1
    p = pp + 1
    two = Modular.normalize n 2
    sub = Modular.subtract n
    mult = Modular.multiply n

propWilliamsNthEqTwo :: Natural -> Natural -> Natural -> Random.Random -> Bool
propWilliamsNthEqTwo pp a mp rnd =
    Lucas.williamsNth two sub mult a m == two
  where
    p = nextPrime (pp + 3) rnd
    d = sub (mult a a) 4
    t = case Quadratic.jacobiSymbol p d of
          Quadratic.Zero -> 2
          Quadratic.Residue -> p - 1
          Quadratic.NonResidue -> p + 1
    m = mp * t
    two = Modular.normalize p 2
    sub = Modular.subtract p
    mult = Modular.multiply p

checkProp :: QuickCheck.Testable prop => String -> prop -> IO ()
checkProp s p = check (s ++ "\n  ") p

main :: IO ()
main =
    do checkProp "Smooth constructor is injective" propSmoothInjective
       checkProp "Modular negate" propModularNegate
       checkProp "Modular invert" propModularInvert
       checkProp "Fermat's little theorem" propFermat
       checkProp "Montgomery invariant" propMontgomeryInvariant
       checkProp "Montgomery normalize" propMontgomeryNormalize
       checkProp "Montgomery reduce" propMontgomeryReduce
       checkProp "Montgomery reduce small" propMontgomeryReduceSmall
       checkProp "Montgomery toNatural" propMontgomeryToNatural
       checkProp "Montgomery fromNatural" propMontgomeryFromNatural
       checkProp "Montgomery zero" propMontgomeryZero
       checkProp "Montgomery one" propMontgomeryOne
       checkProp "Montgomery two" propMontgomeryTwo
       checkProp "Montgomery add" propMontgomeryAdd
       checkProp "Montgomery negate" propMontgomeryNegate
       checkProp "Montgomery multiply" propMontgomeryMultiply
       checkProp "Montgomery modexp" propMontgomeryModexp
       checkProp "Montgomery modexp2" propMontgomeryModexp2
       checkProp "Floor square root" propRootFloor
       checkProp "Ceiling square root" propRootCeiling
       checkProp "Continued fraction square root" propRootContinuedFraction
       checkProp "Jacobi symbol" propJacobiSymbol
       checkProp "Square root modulo prime congruent to 3 mod 4"
         propRootModuloPrime3Mod4
       checkProp "Square root modulo prime congruent to 5 mod 8"
         propRootModuloPrime5Mod8
       checkProp "Square root modulo prime" propRootModuloPrime
       checkProp "Williams sequence" propWilliamsNth
       checkProp "Williams sequence product" propWilliamsNthProduct
       checkProp "Williams sequence equals two" propWilliamsNthEqTwo
       return ()
