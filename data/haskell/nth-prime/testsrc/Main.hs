{- |
module: Main
description: Testing packages exported from OpenTheory
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified Data.List as List
import OpenTheory.Primitive.Natural
import qualified OpenTheory.Natural.Prime as Prime

newtype Sieve =
  Sieve { unSieve :: (Natural,[(Natural,Natural)]) }

perimeter :: Sieve -> Natural
perimeter s = fst (unSieve s)

initial :: Sieve
initial = Sieve (1,[])

next :: Sieve -> (Natural,Sieve)
next s =
    let (b, s') = increment s in
    if b then (perimeter s', s') else next s'

increment :: Sieve -> (Bool,Sieve)
increment =
  \s ->
    let (n,ps) = unSieve s in
    let n' = n + 1 in
    let ps' = inc ps in
    let b = check ps' in
    let ps'' = if b then ps' ++ [(n',0)] else ps' in
    (b, Sieve (n', ps''))
  where
    inc = map (\(p,k) -> (p, (k + 1) `mod` p))

    check = List.all (\(_,k) -> k /= 0)

primes :: [Natural]
primes = List.unfoldr (Just . next) initial

testInitialPrimes :: IO ()
testInitialPrimes =
    let k = 20 in
    let b = take k Prime.all ==
              [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71] in
    if b
      then putStrLn $ "  checked initial " ++ show k ++ " primes"
      else error $ "wrong initial " ++ show k ++ " primes"

checkNthPrime :: Int -> IO ()
checkNthPrime k =
    let p' = primes !! k in
    let p = Prime.all !! k in
    if p == p'
      then putStrLn $ "  checked " ++ show k ++ "th prime is " ++ show p
      else error $ "wrong " ++ show k ++ "th prime"

main :: IO ()
main =
    do testInitialPrimes
       checkNthPrime 100
       checkNthPrime 1000
       return ()
