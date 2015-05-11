{- |
module: Main
description: Convert a chess position from FEN notation to Unicode characters
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified System.Environment as Environment
import System.Console.GetOpt

import qualified Unicode
import qualified Chess

data Options = Options
    {optEdge :: Chess.Edge}
  deriving Show

defaultOptions :: Options
defaultOptions =
  Options
    {optEdge = Chess.SingleEdge}

options :: [OptDescr (Options -> Options)]
options =
    [Option ['e'] ["edge"]
       (ReqArg (\ n opts -> opts {optEdge = Chess.stringToEdge n}) "{0,1,2}")
       "board edge width"]

processOptions :: [String] -> (Options,String)
processOptions args =
    case getOpt Permute options args of
      (o,[f],[]) -> (foldl (flip id) defaultOptions o, f)
      (_,_,errs) -> error $ concat errs ++ usageInfo header options
  where
    header = "Usage: fen2s [OPTION...] FEN"

main :: IO ()
main =
    do args <- Environment.getArgs
       let (opts,fen) = processOptions args
       let edge = optEdge opts
       Unicode.encode (Chess.fenToUnicode fen edge ++ Unicode.newline)
