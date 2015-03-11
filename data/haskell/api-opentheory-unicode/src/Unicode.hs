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
import qualified Data.Word as Word

import qualified OpenTheory.Unicode.UTF8 as UTF8
import OpenTheory.Unicode

instance Show Unicode where
  show = show . Char.chr . fromIntegral . unUnicode

decodeFile :: FilePath -> IO [Either Word.Word8 Unicode]
decodeFile f =
    do b <- ByteString.readFile f
       return (UTF8.decode (ByteString.unpack b))
