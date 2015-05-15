{- |
module: Main
description: Computing modular exponentiation
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified Data.List as List
import System.Console.GetOpt
import qualified System.Environment as Environment
import qualified System.Random
import OpenTheory.Primitive.Natural
import qualified OpenTheory.Primitive.Random as Random
import qualified OpenTheory.Natural.Uniform as Uniform

import qualified Modexp
import qualified Montgomery
import qualified Prime

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

getPrefixString :: String -> (a -> String) -> [a] -> String -> a
getPrefixString k p xs s =
    case filter (List.isPrefixOf s . p) xs of
      [] -> usage $ "bad " ++ k ++ " name: " ++ s
      [x] -> x
      _ : _ : _ -> usage $ "ambiguous " ++ k ++ " name: " ++ s

setToString :: (a -> String) -> [a] -> String
setToString p xs = "{" ++ List.intercalate "," (map p xs) ++ "}"

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

data Operation =
    Modexp
  | Timelock
  deriving Show

operations :: [Operation]
operations = [Modexp,Timelock]

operationToString :: Operation -> String
operationToString oper =
   case oper of
     Modexp -> "modexp"
     Timelock -> "timelock"

stringToOperation :: String -> Operation
stringToOperation = getPrefixString "operation" operationToString operations

--------------------------------------------------------------------------------
-- Algorithms
--------------------------------------------------------------------------------

data Algorithm =
    Naive
  | Montgomery
  deriving Show

algorithms :: [Algorithm]
algorithms = [Naive,Montgomery]

algorithmToString :: Algorithm -> String
algorithmToString oper =
   case oper of
     Naive -> "naive"
     Montgomery -> "montgomery"

stringToAlgorithm :: String -> Algorithm
stringToAlgorithm = getPrefixString "algorithm" algorithmToString algorithms

--------------------------------------------------------------------------------
-- Natural number inputs
--------------------------------------------------------------------------------

data InputNatural =
    Fixed Natural
  | Width Int
  deriving Show

stringToInputNatural :: String -> InputNatural
stringToInputNatural s =
    case s of
      '[' : s' -> case reads s' of
                    [(w,"]")] -> Width w
                    _ -> usage "bad N argument"
      _ -> case reads s of
            [(n,"")] -> Fixed n
            _ -> usage "bad N argument"

uniformInputNatural :: InputNatural -> Random.Random -> Natural
uniformInputNatural (Fixed n) _ = n
uniformInputNatural (Width w) r = Uniform.random (2 ^ w) r

oddInputNatural :: InputNatural -> Random.Random -> Natural
oddInputNatural (Fixed n) _ = n
oddInputNatural (Width w) r = Prime.randomOdd w r

getInputs ::
    Operation -> InputNatural -> Maybe InputNatural -> Maybe InputNatural ->
    Random.Random -> (Natural,Natural,Natural)
getInputs oper wn wx wk r =
    (n,x,k)
  where
    n = oddInputNatural wn rn

    x = case wx of
          Nothing -> Uniform.random n rx
          Just w -> uniformInputNatural w rx

    k = case wk of
          Nothing -> case oper of
                       Modexp -> Uniform.random n rk
                       Timelock -> 1000000
          Just w -> uniformInputNatural w rk

    (rn,r') = Random.split r
    (rx,rk) = Random.split r'

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

data Options = Options
    {optOperation :: Operation,
     optAlgorithm :: Algorithm,
     optModulus :: InputNatural,
     optBase :: Maybe InputNatural,
     optExponent :: Maybe InputNatural}
  deriving Show

defaultOptions :: Options
defaultOptions =
  Options
    {optOperation = Modexp,
     optAlgorithm = Montgomery,
     optModulus = Width 50,
     optBase = Nothing,
     optExponent = Nothing}

options :: [OptDescr (Options -> Options)]
options =
    [Option [] ["operation"]
       (ReqArg (\s opts -> opts {optOperation = stringToOperation s}) "OPERATION")
       "select operation",
     Option [] ["algorithm"]
       (ReqArg (\s opts -> opts {optAlgorithm = stringToAlgorithm s}) "ALGORITHM")
       "select algorithm",
     Option [] ["modulus"]
       (ReqArg (\s opts -> opts {optModulus = stringToInputNatural s}) "N")
       "select modulus",
     Option [] ["base"]
       (ReqArg (\s opts -> opts {optBase = Just (stringToInputNatural s)}) "N")
       "select base",
     Option [] ["exponent"]
       (ReqArg (\s opts -> opts {optExponent = Just (stringToInputNatural s)}) "N")
       "select exponent"]

processOptions :: [String] -> Either [String] (Options,[String])
processOptions args =
    case getOpt Permute options args of
      (opts,work,[]) -> Right (foldl (flip id) defaultOptions opts, work)
      (_,_,errs) -> Left errs

processArguments :: [String] -> Options
processArguments args =
    case processOptions args of
      Left errs -> usage (concat errs)
      Right (opts,work) ->
        case work of
          [] -> opts
          _ : _ -> usage "too many arguments"

usage :: String -> a
usage err =
    error $ err ++ "\n" ++ usageInfo header options ++ footer
  where
    header = "Usage: modexp [OPTION...]"

    footer =
      "where OPERATION is one of " ++
      setToString operationToString operations ++ ",\n" ++
      "ALGORITHM is one of " ++
      setToString algorithmToString algorithms ++ ",\n" ++
      "and N is either a natural number or has the form [bitwidth]."

--------------------------------------------------------------------------------
-- Computation
--------------------------------------------------------------------------------

type Computation = Natural -> Natural -> Natural -> Natural

computation :: Operation -> Algorithm -> Computation
computation Modexp Naive = Modexp.modexp
computation Modexp Montgomery = Montgomery.modexp
computation Timelock Naive = Modexp.modexp2
computation Timelock Montgomery = Montgomery.modexp2

computationToString ::
    Operation -> Natural -> Natural -> Natural -> Natural -> String
computationToString Modexp n x k y =
    "( " ++ show x ++ " ^ " ++ show k ++ " ) `mod` " ++
    show n ++ " == " ++ show y
computationToString Timelock n x k y =
    "( " ++ show x ++ " ^ 2 ^ " ++ show k ++ " ) `mod` " ++
    show n ++ " == " ++ show y

--------------------------------------------------------------------------------
-- Main program
--------------------------------------------------------------------------------

main :: IO ()
main =
    do args <- Environment.getArgs
       r <- fmap Random.fromInt System.Random.randomIO
       let opts = processArguments args
       let oper = optOperation opts
       let (n,x,k) = getInputs oper (optModulus opts) (optBase opts)
                       (optExponent opts) r
       let y = computation oper (optAlgorithm opts) n x k
       putStrLn $ computationToString oper n x k y
       return ()
