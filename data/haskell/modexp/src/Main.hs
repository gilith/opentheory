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
import qualified System.Environment as Environment
import System.Console.GetOpt

import OpenTheory.Primitive.Natural
import qualified ModExp
import qualified Montgomery

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

stringToOperation :: String -> (String,[(String,Algorithm)])
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

stringToAlgorithm :: String -> (String,[(String,Algorithm)])
stringToAlgorithm = getPrefixString "algorithm" algorithmToString algorithms

--------------------------------------------------------------------------------
-- Natural number inputs
--------------------------------------------------------------------------------

data NaturalWidth =
    Natural Natural
  | Width Int
  deriving Show

stringToNaturalWidth :: String -> NaturalWidth
stringToNaturalWidth s =
    case s of
      '[' : s' -> case reads s' of
                    [(w,"]")] -> Width w
                    _ -> usage "bad N argument"
      _ -> case reads s of
            [(n,"")] -> Natural n
            _ -> usage "bad N argument"

getInputs ::
    Operation -> NaturalWidth -> Maybe NaturalWidth -> Maybe NaturalWidth ->
    (Natural,Natural,Natural)
getInputs oper nw xw kw = undefined

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

data Options = Options
    {optOperation :: Operation,
     optAlgorithm :: Algorithm,
     optModulus :: NaturalWidth,
     optBase :: Maybe NaturalWidth,
     optExponent :: Maybe NaturalWidth}
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
       (ReqArg (\s opts -> opts {optModulus = stringToNaturalWidth s}) "N")
       "select modulus",
     Option [] ["base"]
       (ReqArg (\s opts -> opts {optBase = Just (stringToNaturalWidth s)}) "N")
       "select base",
     Option [] ["exponent"]
       (ReqArg (\s opts -> opts {optBase = Just (stringToNaturalWidth s)}) "N")
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
      "where OPERATION is one of\n" ++
      "  {" ++ List.intercalate "," (map fst operations) ++ "},\n" ++
      "ALGORITHM is one of\n" ++
      "  {" ++ List.intercalate "," algorithms ++ "},\n" ++
      "and N is either an integer or of the form [bitwidth]."

--------------------------------------------------------------------------------
-- Computation
--------------------------------------------------------------------------------

type Computation = Natural -> Natural -> Natural -> Natural

computation :: Operation -> Algorithm -> Computation
computation Modexp Naive = ModExp.modExp
computation Modexp Montgomery = Montgomery.modExp
computation Timelock Naive = ModExp.modDoubleExp
computation Timelock Montgomery = Montgomery.modDoubleExp

--------------------------------------------------------------------------------
-- Main program
--------------------------------------------------------------------------------

main :: IO ()
main =
    do args <- Environment.getArgs
       let opts = processArguments args
       let oper = optOperation opts
       let f = computation oper (optAlgorithm opts)
       let (n,x,k) = inputArguments oper (optModulus opts) (optBase opts) (optExponent opts)
       let y = f n x k
       return ()
