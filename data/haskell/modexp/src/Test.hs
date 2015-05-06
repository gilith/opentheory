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
import qualified OpenTheory.Natural.Bits as Bits
import qualified Prime

main :: IO ()
main =
    do s <- System.Random.randomIO
       let w = 512
       let r = Random.fromInt s
       let p = Prime.randomPrime w r
       putStrLn (show p)
       putStrLn (show (Bits.width p))
       return ()
