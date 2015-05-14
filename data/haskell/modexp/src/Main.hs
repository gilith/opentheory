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

data Operation =
    ModExp
  | ModDoubleExp
  deriving Show

type Algorithm = Natural -> Natural -> Natural -> Natural

data NaturalWidth =
    Natural Natural
  | Width Int
  deriving Show

algorithms :: [String]
algorithms =
    ["naive",
     "montgomery"]

modexpAlgorithms :: [(String,Algorithm)]
modexpAlgorithms =
    zip algorithms
      [ModExp.modExp,
       Montgomery.modExp]

timelockAlgorithms :: [(String,Algorithm)]
timelockAlgorithms =
    zip algorithms
      [ModExp.modDoubleExp,
       Montgomery.modDoubleExp]

operations :: [(String,[(String,Algorithm)])]
operations =
    [("modexp",modexpAlgorithms),
     ("timelock",timelockAlgorithms)]

getPrefixString :: String -> [(String,a)] -> String -> (String,a)
getPrefixString k xs s =
    case filter (List.isPrefixOf s . fst) xs of
      [] -> usage $ "bad " ++ k ++ " name: " ++ s
      [x] -> x
      _ : _ : _ -> usage $ "ambiguous " ++ k ++ " name: " ++ s

stringToAlgorithm :: [(String,Algorithm)] -> String -> (String,Algorithm)
stringToAlgorithm = getPrefixString "algorithm"

stringToOperation :: String -> (String,[(String,Algorithm)])
stringToOperation = getPrefixString "operation" operations

stringToNaturalWidth :: String -> NaturalWidth
stringToNaturalWidth s =
    case s of
      '[' : s' -> case reads s' of
                    [(w,"]")] -> Width w
                    _ -> usage "bad N argument"
      _ -> case reads s of
            [(n,"")] -> Natural n
            _ -> usage "bad N argument"

data Options = Options
    {optOperation :: String,
     optAlgorithm :: String,
     optModulus :: NaturalWidth,
     optBase :: Maybe NaturalWidth,
     optExponent :: Maybe NaturalWidth}
  deriving Show

defaultOptions :: Options
defaultOptions =
  Options
    {optOperation = fst (head operations),
     optAlgorithm = last algorithms,
     optModulus = Width 50,
     optBase = Nothing,
     optExponent = Nothing}

options :: [OptDescr (Options -> Options)]
options =
    [Option [] ["operation"]
       (ReqArg (\s opts -> opts {optOperation = s}) "OPERATION")
       "select operation",
     Option [] ["algorithm"]
       (ReqArg (\s opts -> opts {optAlgorithm = s}) "ALGORITHM")
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

main :: IO ()
main =
    do args <- Environment.getArgs
       let opts = processArguments args
       return ()
