{- |
module: Main
description: Timing simple sieve computation of the nth prime
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified System.Environment

import OpenTheory.Primitive.Natural
import qualified OpenTheory.Natural.Prime as Prime

parseArgs :: [String] -> (Bool,Int)
parseArgs a =
    let (b,a') =
          case a of
            "-O0" : l -> (True,l)
            _ -> (False,a) in
    case a' of
      [n] -> (b, fromIntegral (read n :: Integer))
      _ -> error "bad arguments"

main :: IO ()
main =
    do args <- System.Environment.getArgs
       let (s,k) = parseArgs args
       let p = Prime.all !! k
       putStrLn $ "  " ++ show k ++ "th prime is " ++ show p
       return ()
