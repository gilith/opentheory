{- |
module: Main
description: Computing natural number arithmetic operations
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

import Arithmetic.Random
import qualified Arithmetic.Prime.Factor as Factor
import qualified Arithmetic.Modular as Modular
import qualified Arithmetic.Montgomery as Montgomery
import qualified Arithmetic.Williams as Williams

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
    Factor
  | Modexp
  | Timelock
  deriving Show

operations :: [Operation]
operations = [Factor,Modexp,Timelock]

operationToString :: Operation -> String
operationToString oper =
   case oper of
     Factor -> "factor"
     Modexp -> "modexp"
     Timelock -> "timelock"

operationsToString :: [Operation] -> String
operationsToString = setToString operationToString

stringToOperation :: String -> Operation
stringToOperation = getPrefixString "operation" operationToString operations

getOperation :: [String] -> (Operation,[String])
getOperation args =
    case args of
      [] -> usage "no operation specified"
      h : t -> (stringToOperation h, t)

--------------------------------------------------------------------------------
-- Algorithms
--------------------------------------------------------------------------------

data Algorithm =
    Modular
  | Montgomery
  | Williams
  deriving Show

algorithms :: [Algorithm]
algorithms = [Modular,Montgomery,Williams]

possibleAlgorithms :: Operation -> [Algorithm]
possibleAlgorithms Factor = [Williams]
possibleAlgorithms Modexp = [Modular,Montgomery]
possibleAlgorithms Timelock = [Modular,Montgomery]

defaultAlgorithm :: Operation -> Algorithm
defaultAlgorithm = last . possibleAlgorithms

algorithmToString :: Algorithm -> String
algorithmToString oper =
   case oper of
     Modular -> "modular"
     Montgomery -> "montgomery"
     Williams -> "williams"

algorithmsToString :: [Algorithm] -> String
algorithmsToString = setToString algorithmToString

stringToAlgorithm :: String -> Algorithm
stringToAlgorithm = getPrefixString "algorithm" algorithmToString algorithms

--------------------------------------------------------------------------------
-- Natural number inputs
--------------------------------------------------------------------------------

data InputNatural =
    Fixed Natural
  | Width Natural
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

widthInputNatural :: InputNatural -> Random.Random -> Natural
widthInputNatural (Fixed n) _ = n
widthInputNatural (Width w) r = randomWidth w r

oddInputNatural :: InputNatural -> Random.Random -> Natural
oddInputNatural (Fixed n) _ = n
oddInputNatural (Width w) r = randomOdd w r

rsaInputNatural :: InputNatural -> Random.Random -> Natural
rsaInputNatural (Fixed n) _ = n
rsaInputNatural (Width w) rnd = Factor.toNatural (Factor.randomRSA w rnd)

getInput :: Operation -> String -> Maybe InputNatural -> InputNatural
getInput oper s m =
    case m of
      Just n -> n
      Nothing -> usage $ "specify " ++ s ++ " parameter for " ++
                         operationToString oper ++ " operation"

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

data Options = Options
    {optOperation :: Operation,
     optA :: Algorithm,
     optN :: Maybe InputNatural,
     optX :: Maybe InputNatural,
     optK :: Maybe InputNatural}
  deriving Show

nullOptions :: Options
nullOptions =
  Options
    {optOperation = Factor,
     optA = Williams,
     optN = Nothing,
     optX = Nothing,
     optK = Nothing}

options :: [OptDescr (Options -> Options)]
options =
    [Option ['a'] []
       (algorithmArg (\alg opts -> opts {optA = alg}))
       "select algorithm",
     Option ['n'] []
       (inputNaturalArg (\n opts -> opts {optN = n}))
       "select n parameter",
     Option ['x'] []
       (inputNaturalArg (\x opts -> opts {optX = x}))
       "select x parameter",
     Option ['k'] []
       (inputNaturalArg (\k opts -> opts {optK = k}))
       "select k parameter"]
  where
    algorithmArg f = ReqArg (\s -> f (stringToAlgorithm s)) "ALGORITHM"
    inputNaturalArg f =
        ReqArg (\s -> f (Just (stringToInputNatural s))) "NATURAL"

processOptions :: Options -> [String] -> Either [String] (Options,[String])
processOptions opts args =
    case getOpt Permute options args of
      (opts',args',[]) -> Right (foldl (flip id) opts opts', args')
      (_,_,errs) -> Left errs

processOperation :: Options -> Operation -> Options
processOperation opts oper =
    opts {optOperation = oper, optA = defaultAlgorithm oper}

usage :: String -> a
usage err =
    error $ err ++ "\n" ++ usageInfo header options ++ footer
  where
    header = "Usage: arithmetic OPERATION [OPTION...]"

    footer =
      "where OPERATION is one of " ++ operationsToString operations ++ ",\n" ++
      "  ( factor.........factorize n                       )\n" ++
      "  ( modexp.........compute (x ^ k) `mod` n           )\n" ++
      "  ( timelock.......compute (x ^ 2 ^ k) `mod` n       )\n" ++
      "ALGORITHM is one of " ++ algorithmsToString algorithms ++ ",\n" ++
      "  ( modular........naive modular arithmetic          )\n" ++
      "  ( montgomery.....Montgomery multiplication         )\n" ++
      "  ( williams.......Williams p+1 factorization method )\n" ++
      "and NATURAL is either a natural number or has the form [bitwidth]."

usageOperation :: Operation -> a
usageOperation oper =
    error $ err ++ "\n" ++ usageInfo header options ++ footer
  where
    err = "bad algorithm"

    algs = possibleAlgorithms oper

    header = "Usage: arithmetic " ++ operationToString oper ++ " [OPTION...]"

    footer =
      "where ALGORITHM is one of " ++ algorithmsToString algs ++ ",\n" ++
      "and NATURAL is either a natural number or has the form [bitwidth]."

--------------------------------------------------------------------------------
-- Computation
--------------------------------------------------------------------------------

computeFactorWilliams :: Options ->
                         Natural -> Random.Random -> Maybe Factor.Factor
computeFactorWilliams opts n rnd =
    Factor.factor 1000 (Williams.factor x k) n r3
  where
    x = case optX opts of
          Nothing -> 5
          Just w -> widthInputNatural w r1
    k = case optK opts of
          Nothing -> Nothing
          Just w -> Just (widthInputNatural w r2)
    (r1,r23) = Random.split rnd
    (r2,r3) = Random.split r23

computeFactor :: Operation -> Options -> Random.Random -> String
computeFactor oper opts rnd =
    case x of
      Nothing -> error $ "factorization failed for " ++ show n
      Just f -> show n ++ (if Factor.isPrime f then " is prime"
                           else " == " ++ show f)
  where
    n = rsaInputNatural (getInput oper "n" (optN opts)) r1
    x = case optA opts of
          Williams -> computeFactorWilliams opts n r2
          _ -> usageOperation oper
    (r1,r2) = Random.split rnd

computeModexp :: Operation -> Options -> Random.Random -> String
computeModexp oper opts rnd =
    "( " ++ show x ++ " ^ " ++ show k ++ " ) `mod` " ++ show n ++
    " == " ++ show y
  where
    n = oddInputNatural (getInput oper "n" (optN opts)) r1
    x = case optX opts of
          Nothing -> Uniform.random n r2
          Just w -> widthInputNatural w r2
    k = case optK opts of
          Nothing -> Uniform.random n r3
          Just w -> widthInputNatural w r3
    f = case optA opts of
          Modular -> Modular.exp
          Montgomery -> Montgomery.modexp
          _ -> usageOperation oper
    y = f n x k
    (r1,r23) = Random.split rnd
    (r2,r3) = Random.split r23

computeTimelock :: Operation -> Options -> Random.Random -> String
computeTimelock oper opts rnd =
    "( " ++ show x ++ " ^ 2 ^ " ++ show k ++ " ) `mod` " ++ show n ++
     " == " ++ show y
  where
    n = oddInputNatural (getInput oper "n" (optN opts)) r1
    x = case optX opts of
          Nothing -> Uniform.random n r2
          Just w -> widthInputNatural w r2
    k = widthInputNatural (getInput oper "k" (optK opts)) r3
    f = case optA opts of
          Modular -> Modular.exp2
          Montgomery -> Montgomery.modexp2
          _ -> usageOperation oper
    y = f n x k
    (r1,r23) = Random.split rnd
    (r2,r3) = Random.split r23

compute :: Options -> Random.Random -> String
compute opts =
    case oper of
      Factor -> computeFactor oper opts
      Modexp -> computeModexp oper opts
      Timelock -> computeTimelock oper opts
  where
    oper = optOperation opts

--------------------------------------------------------------------------------
-- Main program
--------------------------------------------------------------------------------

processArguments :: [String] -> Options
processArguments cmd =
    case processOptions opts args of
      Left errs -> usage (concat errs)
      Right (opts',work) ->
        case work of
          [] -> opts'
          _ : _ -> usage "too many arguments"
  where
    (oper,args) = getOperation cmd
    opts = processOperation nullOptions oper

main :: IO ()
main =
    do args <- Environment.getArgs
       rnd <- fmap Random.fromInt System.Random.randomIO
       let opts = processArguments args
       putStrLn $ compute opts rnd
       return ()
