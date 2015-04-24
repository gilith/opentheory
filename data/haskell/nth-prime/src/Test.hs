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

import qualified NaiveSieve
import qualified OpenTheory.Natural.Prime as OptimizedNaiveSieve
import qualified OpenTheory.Primitive.Natural as Natural

sieves :: [[Natural.Natural]]
sieves = [NaiveSieve.primes,OptimizedNaiveSieve.primes]

checkInitialPrimes :: Int -> IO ()
checkInitialPrimes k =
    case map (take k) sieves of
      [] -> error "no sieves defined"
      l : ls ->
        if all ((==) l) ls
          then putStrLn $ "  checked initial " ++ show k ++ " primes are " ++ show l
          else error $ "wrong initial " ++ show k ++ " primes"

checkNthPrime :: Int -> IO ()
checkNthPrime k =
    case map (\s -> s !! k) sieves of
      [] -> error "no sieves defined"
      p : ps ->
        if all ((==) p) ps
          then putStrLn $ "  checked prime[" ++ show k ++ "] is " ++ show p
          else error $ "wrong prime[" ++ show k ++ "]"

main :: IO ()
main =
    do checkInitialPrimes 10
       checkNthPrime 100
       checkNthPrime 1000
       return ()
