{- |
module: Unicode
description: Unicode character API
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Unicode
  ( decodeFile )
where

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Char as Char

import qualified OpenTheory.Data.Unicode.UTF8 as UTF8
import OpenTheory.Data.Unicode

instance Show Unicode where
  show = show . Char.chr . unUnicode

decodeFile :: FilePath -> IO [Either Data.Word.Word8 Unicode]
decodeFile f =
    do b <- ByteString.readFile f
       return (UTF8.decode b)
