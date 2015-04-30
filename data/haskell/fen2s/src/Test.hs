{- |
module: Main
description: Testing the FEN to Unicode conversion
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified Unicode
import qualified Chess

tests :: [(String,String,Chess.Edge)]
tests =
    [("Initial position",
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR",
      Chess.NoEdge),
     ("Mate in 3",
      "KR6/8/kN4r1/p7/8/8/8/8",
      Chess.SingleEdge),
     ("Mate in 4",
      "8/8/4K3/1BkN4/8/2NpP3/3P4/8",
      Chess.DoubleEdge)]

outputFen :: (String,String,Chess.Edge) -> IO ()
outputFen (s,f,e) =
    do putStrLn (s ++ ":")
       Unicode.encode (Chess.fenToUnicode f e ++ Unicode.newline)

main :: IO ()
main = mapM_ outputFen tests
