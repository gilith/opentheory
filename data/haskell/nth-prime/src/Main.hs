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

import qualified Data.List as List
import qualified System.Environment as Environment
import System.Console.GetOpt

import qualified OpenTheory.Primitive.Natural as Natural
import qualified NaiveSieve
import qualified OpenTheory.Natural.Prime as OptimizedNaiveSieve

type Sieve = [Natural.Natural]

verifiedSieve :: (String,Sieve)
verifiedSieve = ("verified",OptimizedNaiveSieve.primes)

sieves :: [(String,Sieve)]
sieves =
  [verifiedSieve,
   ("naive",NaiveSieve.primes),
   ("optimizedNaive",OptimizedNaiveSieve.primes)]

stringToSieve :: String -> (String,Sieve)
stringToSieve s =
  case filter (List.isPrefixOf s . fst) sieves of
    [] -> usage $ "bad sieve name: " ++ s
    [x] -> x
    _ : _ : _ -> usage $ "ambiguous sieve name: " ++ s

data Options = Options
    {optSieve :: String}
  deriving Show

defaultOptions :: Options
defaultOptions =
  Options
    {optSieve = fst verifiedSieve}

options :: [OptDescr (Options -> Options)]
options =
    [Option ['s'] ["sieve"]
       (ReqArg (\s opts -> opts {optSieve = s}) "SIEVE")
       "select sieve algorithm"]

processOptions :: [String] -> Either [String] (Options,[String])
processOptions args =
    case getOpt Permute options args of
      (opts,work,[]) -> Right(foldl (flip id) defaultOptions opts, work)
      (_,_,errs) -> Left errs

processArguments :: [String] -> (Options,Int)
processArguments args =
    case processOptions args of
      Left errs -> usage (concat errs)
      Right (opts,work) ->
        let ns = case work of
                   [] -> usage "not enough arguments"
                   [x] -> x
                   _ : _ : _ -> usage "too many arguments" in
        let n = case reads ns of
                  [(x,"")] -> x
                  _ -> usage "bad N argument" in
        (opts,n)

usage :: String -> a
usage err =
    error $ err ++ "\n" ++ usageInfo header options ++ footer
  where
    header = "Usage: nth-prime [OPTION...] N"

    footer =
      "where N is an integer and SIEVE is one of\n" ++
      "  {" ++ List.intercalate "," (map fst sieves) ++ "}"

main :: IO ()
main =
    do args <- Environment.getArgs
       let (opts,n) = processArguments args
       let (name,primes) = stringToSieve (optSieve opts)
       let p = primes !! n
       putStrLn $ "  prime[" ++ show n ++ "] = " ++ show p ++ "  (using " ++ name ++ " sieve)"
       return ()
