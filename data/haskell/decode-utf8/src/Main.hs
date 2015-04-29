{- |
module: Main
description: Decode a UTF-8 byte stream on standard input
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified Data.Word as Word

import OpenTheory.Unicode
import qualified Unicode

info :: Either Word.Word8 Unicode -> String
info (Left b) = "invalid byte " ++ show b
info (Right c) =
    "valid unicode character " ++ show (unUnicode c) ++ " " ++
    Unicode.toString c

display :: Either Word.Word8 Unicode -> IO ()
display c = putStrLn (info c)

main :: IO ()
main =
    do cs <- Unicode.decode
       mapM_ display cs
