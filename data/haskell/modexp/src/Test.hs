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

import qualified System.Random

import OpenTheory.Primitive.Natural
import qualified OpenTheory.Primitive.Random as Random
import qualified OpenTheory.Natural.Uniform as Uniform
import qualified Egcd
import qualified Prime
import qualified ModExp

parameters :: Int -> Random.Random -> (Natural,Natural,Natural)
parameters w r =
    (n,x,k)
  where
    n = Prime.randomPrime w r1
    x = Uniform.random n r2
    k = Uniform.random n r3
    (r1,r23) = Random.split r
    (r2,r3) = Random.split r23

main :: IO ()
main =
    do s <- System.Random.randomIO
       let w = 64
       let r = Random.fromInt s
       let (n,x,k) = parameters w r
       let j = n - 1
       let y = ModExp.modExp n x k
       let z = ModExp.modExp n y j
       putStrLn (show x ++ " ^ " ++ show k ++ " `mod` " ++ show n ++ " == " ++ show y)
       putStrLn (show y ++ " ^ " ++ show j ++ " `mod` " ++ show n ++ " == " ++ show z)
       return ()
