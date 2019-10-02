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

import OpenTheory.Primitive.Natural
import OpenTheory.Natural
import OpenTheory.Natural.Divides
import qualified OpenTheory.Natural.Bits as Bits
import qualified Data.Maybe as Maybe
import qualified OpenTheory.Natural.Prime as Prime
import qualified Test.QuickCheck as QuickCheck
import qualified OpenTheory.Primitive.Random as Random
import qualified OpenTheory.Natural.Uniform as Uniform

import Arithmetic.Random
import Arithmetic.Prime
import qualified Arithmetic.ContinuedFraction as ContinuedFraction
import qualified Arithmetic.Prime.Factor as Factor
import qualified Arithmetic.Modular as Modular
import qualified Arithmetic.Montgomery as Montgomery
import qualified Arithmetic.Pell as Pell
import qualified Arithmetic.Polynomial as Polynomial
import qualified Arithmetic.Quadratic as Quadratic
import qualified Arithmetic.Ring as Ring
import qualified Arithmetic.Williams as Williams

propPrimes :: Natural -> Bool
propPrimes k =
    primes !! (fromIntegral k) ==
    Prime.primes !! (fromIntegral k)

propRandomPrime :: Natural -> Random.Random -> Bool
propRandomPrime wp rnd =
    Bits.width p == w &&
    isPrime p r2
  where
    w = wp + 2
    p = randomPrime w r1
    (r1,r2) = Random.split rnd

propRandomRSA :: Natural -> Random.Random -> Bool
propRandomRSA wp rnd =
    Bits.width n == w &&
    not (isPrime n r2)
  where
    w = wp + 5
    n = Factor.toNatural (Factor.randomRSA w r1)
    (r1,r2) = Random.split rnd

propTrialDivision :: Natural -> Natural -> Bool
propTrialDivision k np =
    Factor.toNatural f * m == n &&
    all (\p -> not (divides p m)) ps
  where
    n = np + 1
    ps = take (fromIntegral k) primes
    (f,m) = Factor.trialDivision ps n

propModularNegate :: Natural -> Random.Random -> Bool
propModularNegate np rnd =
    Modular.add n a b == 0 &&
    b < n
  where
    n = np + 1
    a = Uniform.random n rnd
    b = Modular.negate n a

propModularSubtract :: Natural -> Natural -> Natural -> Bool
propModularSubtract np a b =
    Modular.subtract n a b ==
    Ring.subtract r (Ring.fromNatural r a) (Ring.fromNatural r b)
  where
    n = np + 1
    r = Modular.ring n

propModularExp2 :: Natural -> Natural -> Natural -> Bool
propModularExp2 np x k =
    Modular.exp2 n x k == Ring.exp2 r (Ring.fromNatural r x) k
  where
    n = np + 1
    r = Modular.ring n

propModularDivide :: Natural -> Natural -> Natural -> Bool
propModularDivide np a b =
    case Modular.divide n a b of
      Nothing -> not (Modular.divides n a b)
      Just c -> Modular.multiply n b c == Modular.normalize n a && c < n
  where
    n = np + 1

propModularDivides :: Natural -> Natural -> Natural -> Bool
propModularDivides np a b =
    Modular.divides n a b ==
    Ring.divides r (Ring.fromNatural r a) (Ring.fromNatural r b)
  where
    n = np + 1
    r = Modular.ring n

propModularInvert :: Natural -> Natural -> Bool
propModularInvert np a =
    Modular.invert n a == Ring.invert r (Ring.fromNatural r a)
  where
    n = np + 1
    r = Modular.ring n

propFermat :: Natural -> Random.Random -> Bool
propFermat pp rnd =
    Modular.exp p a p == a
  where
    p = nextPrime (pp + 3) r1
    a = Uniform.random p r2
    (r1,r2) = Random.split rnd

propMontgomeryInvariant :: Natural -> Natural -> Bool
propMontgomeryInvariant np b =
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
       Montgomery.zParameters = z} =
      Montgomery.customParameters (2 * np + 3) (Bits.width n + b)
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
    Williams.sequence one two sub mult p !! (fromIntegral k) ==
    Williams.nth two sub mult p k
  where
    n = np + 1
    one = 1
    two = 2
    sub = Modular.subtract n
    mult = Modular.multiply n

propWilliamsNthProduct :: Natural -> Natural -> Natural -> Natural -> Bool
propWilliamsNthProduct np pp i j =
    Williams.nth two sub mult p (i * j) ==
    Williams.nth two sub mult (Williams.nth two sub mult p i) j
  where
    n = np + 1
    p = pp + 1
    two = Modular.normalize n 2
    sub = Modular.subtract n
    mult = Modular.multiply n

propWilliamsNthExp :: Natural -> Natural -> Natural -> Natural -> Bool
propWilliamsNthExp np p m k =
    Williams.nthExp two sub mult p m k ==
    Williams.nth two sub mult p (m ^ k)
  where
    n = np + 1
    two = Modular.normalize n 2
    sub = Modular.subtract n
    mult = Modular.multiply n

propWilliamsNthEqTwo :: Natural -> Natural -> Natural -> Random.Random -> Bool
propWilliamsNthEqTwo pp a mp rnd =
    Williams.nth two sub mult a m == two
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

propWilliamsFactor :: Natural -> Natural -> Natural -> Random.Random -> Bool
propWilliamsFactor np x k rnd =
    case Williams.factor x (Just k) n rnd of
      Nothing -> True
      Just p -> 1 < p && p < n && divides p n
  where
    n = 2 * np + 5

propPolynomialConstantDegree :: Natural -> Natural -> Bool
propPolynomialConstantDegree np cp =
    Polynomial.degree (Polynomial.constant r c) == if c == 0 then 0 else 1
  where
    n = np + 1
    r = Modular.ring n
    c = Ring.fromNatural r cp

propPolynomialFromNaturalDegree :: Natural -> Natural -> Bool
propPolynomialFromNaturalDegree np c =
    Polynomial.degree (Polynomial.fromNatural r c) ==
    if divides n c then 0 else 1
  where
    n = np + 1
    r = Modular.ring n

propPolynomialAddDegree :: Natural -> [Natural] -> [Natural] -> Bool
propPolynomialAddDegree np ps qs =
    if d_p == d_q then d_pq <= d_p
    else d_pq == max d_p d_q
  where
    n = np + 1
    r = Modular.ring n
    p = Polynomial.fromCoefficients r (map (Ring.fromNatural r) ps)
    q = Polynomial.fromCoefficients r (map (Ring.fromNatural r) qs)
    d_p = Polynomial.degree p
    d_q = Polynomial.degree q
    pq = Polynomial.add p q
    d_pq = Polynomial.degree pq

propPolynomialNegateDegree :: Natural -> [Natural] -> Bool
propPolynomialNegateDegree np ps =
    Polynomial.degree (Polynomial.negate p) == Polynomial.degree p
  where
    n = np + 1
    r = Modular.ring n
    p = Polynomial.fromCoefficients r (map (Ring.fromNatural r) ps)

propPolynomialMultiplyDegree :: Natural -> [Natural] -> [Natural] -> Bool
propPolynomialMultiplyDegree np ps qs =
    if d_p == 0 || d_q == 0 then d_pq == 0
    else d_pq + 1 <= d_p + d_q
  where
    n = np + 1
    r = Modular.ring n
    p = Polynomial.fromCoefficients r (map (Ring.fromNatural r) ps)
    q = Polynomial.fromCoefficients r (map (Ring.fromNatural r) qs)
    d_p = Polynomial.degree p
    d_q = Polynomial.degree q
    pq = Polynomial.multiply p q
    d_pq = Polynomial.degree pq

propPolynomialConstantEvaluate :: Natural -> Natural -> Natural -> Bool
propPolynomialConstantEvaluate np cp xp =
    Polynomial.evaluate (Polynomial.constant r c) x == c
  where
    n = np + 1
    r = Modular.ring n
    c = Ring.fromNatural r cp
    x = Ring.fromNatural r xp

propPolynomialFromNaturalEvaluate :: Natural -> Natural -> Natural -> Bool
propPolynomialFromNaturalEvaluate np c xp =
    Polynomial.evaluate (Polynomial.fromNatural r c) x ==
    Ring.fromNatural r c
  where
    n = np + 1
    r = Modular.ring n
    x = Ring.fromNatural r xp

propPolynomialAddEvaluate ::
    Natural -> [Natural] -> [Natural] -> Natural -> Bool
propPolynomialAddEvaluate np ps qs xp =
    Polynomial.evaluate (Polynomial.add p q) x ==
    Ring.add r (Polynomial.evaluate p x) (Polynomial.evaluate q x)
  where
    n = np + 1
    r = Modular.ring n
    p = Polynomial.fromCoefficients r (map (Ring.fromNatural r) ps)
    q = Polynomial.fromCoefficients r (map (Ring.fromNatural r) qs)
    x = Ring.fromNatural r xp

propPolynomialNegateEvaluate :: Natural -> [Natural] -> Natural -> Bool
propPolynomialNegateEvaluate np ps xp =
    Polynomial.evaluate (Polynomial.negate p) x ==
    Ring.negate r (Polynomial.evaluate p x)
  where
    n = np + 1
    r = Modular.ring n
    p = Polynomial.fromCoefficients r (map (Ring.fromNatural r) ps)
    x = Ring.fromNatural r xp

propPolynomialMultiplyEvaluate ::
    Natural -> [Natural] -> [Natural] -> Natural -> Bool
propPolynomialMultiplyEvaluate np ps qs xp =
    Polynomial.evaluate (Polynomial.multiply p q) x ==
    Ring.multiply r (Polynomial.evaluate p x) (Polynomial.evaluate q x)
  where
    n = np + 1
    r = Modular.ring n
    p = Polynomial.fromCoefficients r (map (Ring.fromNatural r) ps)
    q = Polynomial.fromCoefficients r (map (Ring.fromNatural r) qs)
    x = Ring.fromNatural r xp

propPolynomialQuotientRemainder :: Natural -> [Natural] -> [Natural] -> Bool
propPolynomialQuotientRemainder np ps qs =
    case Polynomial.quotientRemainder p q of
      Nothing -> True
      Just (a,b) -> Polynomial.coefficients p ==
                    Polynomial.coefficients (Polynomial.add (Polynomial.multiply a q) b)
  where
    n = np + 1
    r = Modular.ring n
    p = Polynomial.fromCoefficients r (map (Ring.fromNatural r) ps)
    q = Polynomial.fromCoefficients r (map (Ring.fromNatural r) qs)

propPolynomialQuotientRemainderMonic ::
    Natural -> [Natural] -> [Natural] -> Bool
propPolynomialQuotientRemainderMonic np ps qs =
    Maybe.isJust (Polynomial.quotientRemainder p q)
  where
    n = np + 2
    r = Modular.ring n
    p = Polynomial.fromCoefficients r (map (Ring.fromNatural r) ps)
    q = Polynomial.fromCoefficients r (map (Ring.fromNatural r) (qs ++ [1]))

propPellEquation :: Natural -> Natural -> Bool
propPellEquation n ip =
    a * a == n * b * b + 1
  where
    (a,b) = Pell.solutions n !! i
    i = fromIntegral (if Quadratic.isSquare n then 0 else ip)

check :: QuickCheck.Testable prop => String -> prop -> IO ()
check desc prop =
    do putStr (desc ++ "\n  ")
       res <- QuickCheck.quickCheckWithResult args prop
       case res of
         QuickCheck.Failure {} -> error "Proposition failed"
         _ -> return ()
  where
    args = QuickCheck.stdArgs {QuickCheck.maxSuccess = 1000}

main :: IO ()
main =
    do check "Sieve of Eratosphenes" propPrimes
       check "Generating random primes" propRandomPrime
       check "Generating random RSA moduli" propRandomRSA
       check "Trial division" propTrialDivision
       check "Modular negate" propModularNegate
       check "Modular subtract" propModularSubtract
       check "Modular exp2" propModularExp2
       check "Modular divide" propModularDivide
       check "Modular divides" propModularDivides
       check "Modular invert" propModularInvert
       check "Fermat's little theorem" propFermat
       check "Montgomery invariant" propMontgomeryInvariant
       check "Montgomery normalize" propMontgomeryNormalize
       check "Montgomery reduce" propMontgomeryReduce
       check "Montgomery reduce small" propMontgomeryReduceSmall
       check "Montgomery toNatural" propMontgomeryToNatural
       check "Montgomery fromNatural" propMontgomeryFromNatural
       check "Montgomery zero" propMontgomeryZero
       check "Montgomery one" propMontgomeryOne
       check "Montgomery two" propMontgomeryTwo
       check "Montgomery add" propMontgomeryAdd
       check "Montgomery negate" propMontgomeryNegate
       check "Montgomery multiply" propMontgomeryMultiply
       check "Montgomery modexp" propMontgomeryModexp
       check "Montgomery modexp2" propMontgomeryModexp2
       check "Floor square root" propRootFloor
       check "Ceiling square root" propRootCeiling
       check "Continued fraction square root" propRootContinuedFraction
       check "Jacobi symbol" propJacobiSymbol
       check "Square root modulo prime congruent to 3 mod 4"
         propRootModuloPrime3Mod4
       check "Square root modulo prime congruent to 5 mod 8"
         propRootModuloPrime5Mod8
       check "Square root modulo prime" propRootModuloPrime
       check "Williams sequence" propWilliamsNth
       check "Williams sequence product" propWilliamsNthProduct
       check "Williams sequence exponential" propWilliamsNthExp
       check "Williams sequence equals two" propWilliamsNthEqTwo
       check "Williams factorization works" propWilliamsFactor
       check "Polynomial constant degree" propPolynomialConstantDegree
       check "Polynomial fromNatural degree" propPolynomialFromNaturalDegree
       check "Polynomial add degree" propPolynomialAddDegree
       check "Polynomial negate degree" propPolynomialNegateDegree
       check "Polynomial multiply degree" propPolynomialMultiplyDegree
       check "Polynomial constant evaluate" propPolynomialConstantEvaluate
       check "Polynomial fromNatural evaluate" propPolynomialFromNaturalEvaluate
       check "Polynomial add evaluate" propPolynomialAddEvaluate
       check "Polynomial negate evaluate" propPolynomialNegateEvaluate
       check "Polynomial multiply evaluate" propPolynomialMultiplyEvaluate
       check "Polynomial quotient remainder" propPolynomialQuotientRemainder
       check "Polynomial quotient remainder monic"
         propPolynomialQuotientRemainderMonic
       check "Pell equation solutions" propPellEquation
       return ()
