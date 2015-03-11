{- |
module: Main
description: Testing the computation of the nth prime
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified Prime as Spec
import qualified OpenTheory.Natural.Prime as Prime

checkInitialPrimes :: IO ()
checkInitialPrimes =
    let k = 10 in
    let l = take k Spec.primes in
    if take k Prime.primes == l
      then putStrLn $ "  checked initial " ++ show k ++ " primes are " ++ show l
      else error $ "wrong initial " ++ show k ++ " primes"

checkNthPrime :: Int -> IO ()
checkNthPrime k =
    let p' = Spec.primes !! k in
    let p = Prime.primes !! k in
    if p == p'
      then putStrLn $ "  checked prime[" ++ show k ++ "] is " ++ show p
      else error $ "wrong prime[" ++ show k ++ "]"

main :: IO ()
main =
    do checkInitialPrimes
       checkNthPrime 100
       checkNthPrime 1000
       return ()
