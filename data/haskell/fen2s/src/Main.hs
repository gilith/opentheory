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

import qualified Unicode
import qualified Chess

parseArgs :: [String] -> String
parseArgs a =
    case a of
      [f] -> f
      _ -> error "usage: fen2s FEN"

main :: IO ()
main =
    do args <- Environment.getArgs
       let fen = parseArgs args
       let edge = Chess.DoubleEdge
       Unicode.encode (Chess.fenToUnicode fen edge ++ Unicode.newline)
