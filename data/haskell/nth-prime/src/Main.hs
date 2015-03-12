{- |
module: Main
description: Computing the nth prime
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified System.Environment
import OpenTheory.Natural.Prime

parseArgs :: [String] -> Int
parseArgs a =
    case a of
      [n] -> read n
      _ -> error "usage: nth-prime N"

main :: IO ()
main =
    do args <- System.Environment.getArgs
       let k = parseArgs args
       let p = primes !! k
       putStrLn $ "  prime[" ++ show k ++ "] is " ++ show p
       return ()
